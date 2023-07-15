#!/usr/bin/env bash
set -eo pipefail

aws --profile blog s3 cp index.html s3://071062044970-blog-root/index.html
