#!/usr/bin/env bash
set -eo pipefail

TOKEN=""

while getopts ":t:" o; do
    case "${o}" in
        t)
            TOKEN=${OPTARG}
            ;;
        *)
            usage
            ;;
    esac
done
shift $((OPTIND-1))

if [ -z "${TOKEN}" ]
then
   echo "Must specify a kubernetes token (via -t <token>)" 1>&2
   exit 1
fi

SPARK_HOME=/home/emacdona/.sdkman/candidates/spark/current
DOCKER_REPO_PORT=$(docker ps -f name=registry --format "{{json .}}" | jq -r '.Ports' | perl -ne 'print((split(/:/, (split(/->/))[0]))[1], "\n")')

[ ! -f "${SPARK_HOME}/jars/bcprov-jdk18on-171.jar" ] && (cd "${SPARK_HOME}/jars" && curl -LO https://bouncycastle.org/download/bcprov-jdk18on-171.jar)
[ ! -f "${SPARK_HOME}/jars/bcpkix-jdk18on-171.jar" ] && (cd "${SPARK_HOME}/jars" && curl -LO https://bouncycastle.org/download/bcpkix-jdk18on-171.jar)

"${SPARK_HOME}/bin/docker-image-tool.sh" -r "localhost:${DOCKER_REPO_PORT}/net.edmacdonald" -t latest build
"${SPARK_HOME}/bin/docker-image-tool.sh" -r "localhost:${DOCKER_REPO_PORT}/net.edmacdonald" -t latest push

"${SPARK_HOME}/bin/spark-submit" \
    --master k8s://https://172.17.0.1:42042 \
    --deploy-mode cluster \
    --name spark-pi \
    --class org.apache.spark.examples.SparkPi \
    --conf spark.executor.instances=5 \
    --conf spark.kubernetes.container.image="registry:${DOCKER_REPO_PORT}/net.edmacdonald/spark:latest" \
    --conf spark.kubernetes.authenticate.oauthToken="${TOKEN}" \
    --conf spark.kubernetes.authenticate.submission.oauthToken="${TOKEN}" \
    --conf spark.kubernetes.authenticate.driver.oauthToken="${TOKEN}" \
    "local:///opt/spark/examples/jars/spark-examples_2.12-3.2.1.jar" 100000

