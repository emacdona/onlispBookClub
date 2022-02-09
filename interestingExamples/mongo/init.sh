#!/usr/bin/env zsh
set -eo pipefail

MONGO_PW=""

while getopts ":p:" o; do
    case "${o}" in
        p)
            MONGO_PW=${OPTARG}
            ;;
        *)
            usage
            ;;
    esac
done
shift $((OPTIND-1))

if [ -z "${MONGO_PW}" ]
then
   echo "Must specify a password" 1>&2
   exit 1
fi

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

if [ ! -f "data.json" ]
then
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
else
   echo "Data file already created..."
fi

echo "Importing to mongo..."

mongoimport \
    --authenticationDatabase admin \
    -h 127.0.0.1 \
    -u root -p "$MONGO_PW" \
    -d reddit \
    -c subreddits \
    data.json

echo "Converting dates..."

mongosh --eval "var mongopw='${MONGO_PW}'" --file dateConversion.js
