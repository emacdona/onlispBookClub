#!/usr/bin/env bash
set -eo pipefail

JEKYLL_ENV=production bundle exec jekyll build
aws --profile blog s3 sync ./_site s3://consartist.com
