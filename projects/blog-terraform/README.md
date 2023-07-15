# Blog Terraform

Followed directions here:
https://developer.hashicorp.com/terraform/tutorials/applications/cloudflare-static-website

I wanted to use AWS CloudFront, but it's not enabled on my account. I have an outstanding ticket to increase my 
"Cloud Front Distribution" quota to "1", but I got annoyed that I have to ask them to turn it on, so I went
the Cloudflare route (at least for now anyway).

I installed direnv so I could have it manage my AWS_PROFILE and CLOUDFLARE_API_TOKEN for me. It would be great
if Cloudflare supported temporary credentials as well as AWS does. Maybe they do... it's something I
need to look into.