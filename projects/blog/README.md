
## Getting started
Don't forget to:
```shell
bundle install
```

## Logging into aws (so you can sync)
aws sso login --profile blog

Note: Linux packages for awscli are OLD (1.x). You'll need to install it via non-repo means, likely:
https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html

## Starting jekyll locally
```shell
bundle exec jekyll serve --unpublished
```