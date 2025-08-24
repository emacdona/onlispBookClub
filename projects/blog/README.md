
## Getting started
Don't forget to:
```shell
bundle install
```

## Logging into aws (so you can sync)
aws sso login --profile blog

(Look for "aws_config" elsewhere in this repo. Copy it to "~/.aws/config")

Note: Linux packages for awscli are OLD (1.x). You'll need to install it via non-repo means, likely:
https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html

## Starting jekyll locally
```shell
bundle exec jekyll serve --unpublished
```

## Sync with AWS
```shell
./sync.sh
```

## Running locally / syncing if you're not on Linux
```shell
docker compose up # run locally
docker compose -f docker-compose-rollout.yaml up # sync
```
