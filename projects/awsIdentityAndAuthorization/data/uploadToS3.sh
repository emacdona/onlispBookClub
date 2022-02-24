#!/usr/bin/env bash
set -eo pipefail

usage() { echo "Usage: $0 [-p <bucket name prefix>]" 1>&2; exit 1; }

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

for dirname in s3Data/*; do
    BUCKET_NAME="${PREFIX}-$(basename "${dirname}")"
    aws s3api create-bucket --bucket "${BUCKET_NAME}"

    for file in $(find "${dirname}" -type f); do
        KEY=$(echo "${file}" | perl -pe "s|${dirname}/||")
        aws s3api put-object --bucket "${BUCKET_NAME}" --key "${KEY}" --body "${file}"
    done
done
