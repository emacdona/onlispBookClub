provider "aws" {
  region = "us-east-1"
}

provider "cloudflare" {}

data "aws_caller_identity" "current" {}

locals {
  account_id = data.aws_caller_identity.current.account_id
  domain_name = "consartist.com"
}

resource "aws_s3_bucket" "site" {
  //bucket = "${local.account_id}-blog-root"
  bucket = local.domain_name
}

resource "aws_s3_bucket_website_configuration" "site" {
  bucket = aws_s3_bucket.site.bucket
  index_document {
    suffix = "index.html"
  }

  error_document {
    key = "error.html"
  }
}

resource "aws_s3_bucket_public_access_block" "site"{
  bucket = aws_s3_bucket.site.bucket

  block_public_acls = false
  block_public_policy = false
  ignore_public_acls = false
  restrict_public_buckets = false
}

resource "aws_s3_bucket_public_access_block" "www"{
  bucket = aws_s3_bucket.www.bucket

  block_public_acls = false
  block_public_policy = false
  ignore_public_acls = false
  restrict_public_buckets = false
}

resource "aws_s3_bucket_policy" "site" {
  bucket = aws_s3_bucket.site.bucket
  policy = data.aws_iam_policy_document.allow_public_read_access.json
}

data "aws_iam_policy_document" "allow_public_read_access" {
  statement {
    effect = "Allow"
    principals {
      identifiers = ["*"]
      type = "AWS"
    }
    actions = ["s3:GetObject"]
    resources = ["${aws_s3_bucket.site.arn}/*"]
  }
}

resource "aws_s3_bucket" "www" {
  bucket = "www.${local.domain_name}"
}

resource "aws_s3_bucket_website_configuration" "www" {
  bucket = aws_s3_bucket.www.id

  redirect_all_requests_to {
    host_name = local.domain_name
  }
}

data "cloudflare_zones" "domain" {
  filter {
    name = local.domain_name
  }
}

resource "cloudflare_record" "site_cname" {
  zone_id = data.cloudflare_zones.domain.zones[0].id
  name    = local.domain_name
  value   = aws_s3_bucket_website_configuration.site.website_endpoint
  type    = "CNAME"

  ttl     = 1
  proxied = true
}

resource "cloudflare_record" "www" {
  zone_id = data.cloudflare_zones.domain.zones[0].id
  name    = "www"
  value   = local.domain_name
  type    = "CNAME"

  ttl     = 1
  proxied = true
}

/*
Cloudflare recommends following three records if you don't have MX records. They tell email servers to ignore all
email originating from your domain. I need to read more about it, but I'll take Cloudflare's word for now.
*/

resource "cloudflare_record" "txt1" {
  zone_id = data.cloudflare_zones.domain.zones[0].id
  name    = "consartist.com"
  value   = "v=spf1 -all"
  type    = "TXT"
}

resource "cloudflare_record" "txt2" {
  zone_id = data.cloudflare_zones.domain.zones[0].id
  name    = "*._domainkey"
  value   = "v=DKIM1; p="
  type    = "TXT"
}

resource "cloudflare_record" "txt3" {
  zone_id = data.cloudflare_zones.domain.zones[0].id
  name    = "_dmarc"
  value   = "v=DMARC1; p=reject; sp=reject; adkim=s; aspf=s;"
  type    = "TXT"
}