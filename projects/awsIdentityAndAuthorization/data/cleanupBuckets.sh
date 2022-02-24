#!/usr/bin/env bash
set -eo pipefail

usage() { echo "Usage: $0 -p <bucket name prefix>" 1>&2; exit 1; }

PREFIX="identity.experiment.edmacdonald.net"

while getopts ":p:" o; do
    case "${o}" in
        p)
            PREFIX=${OPTARG}
            ;;
        *)
            usage
            ;;
    esac
done
shift $((OPTIND-1))

if [ -z "${PREFIX}" ]
then
    usage
fi


aws s3api list-buckets \
| jq -r '.Buckets[].Name' \
| grep "^${PREFIX}" \
| xargs -I{} aws s3 rm s3://{} --recursive

aws s3api list-buckets \
| jq -r '.Buckets[].Name' \
| grep "^${PREFIX}" \
| xargs -I{} aws s3api delete-bucket --bucket {}
