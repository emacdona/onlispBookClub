#!/usr/bin/env bash
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

mongorestore \
    -u root \
    -p "${MONGO_PW}" \
    --authenticationDatabase admin \
    -d reddit \
    -c subreddits \
    dump/reddit/subreddits.bson
