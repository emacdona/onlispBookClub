#!/usr/bin/env zsh
set -eo pipefail

RAW_DATA_FILE="reddit_subreddits.ndjson"

if [ ! -f "${RAW_DATA_FILE}.zst" ]
then
    curl -O "http://files.pushshift.io/reddit/subreddits/${RAW_DATA_FILE}.zst"
else
    echo "File found, skipping download..."
fi;

if [ ! -f "${RAW_DATA_FILE}" ]
then
    zstd -d reddit_subreddits.ndjson.zst
else
    echo "File already decompressed..."
fi

MONGO_PW="$(kubectl get secret --namespace default my-release-mongodb -o jsonpath="{.data.mongodb-root-password}" | base64 --decode)"

# effing hell. head causes a non-zero exit code -- by design
# ... if not all records are consumed.
# https://github.com/stedolan/jq/issues/1017
set +e
cat reddit_subreddits.ndjson \
    | head -n 2000000 \
    | jq -c '. |
{
"created_utc": .created_utc,
"display_name": .display_name,
"title": .title,
"url": .url,
"active_user_count": .active_user_count,
"lang": .lang,
"over18": .over18,
"allow_images": .allow_images,
"allow_videos": .allow_videos,
"allow_videogifs": .allow_videogifs,
"public_description": .public_description,
"subscribers": .subscribers,
"header_img": .header_img
}' > data.json
set -e

mongoimport \
    --authenticationDatabase admin \
    -h 127.0.0.1 \
    -u root -p "$MONGO_PW" \
    -d reddit \
    -c subreddits \
    data.json

mongosh --eval "var mongopw='${MONGO_PW}'" --file dateConversion.js
