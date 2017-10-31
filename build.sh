#!/usr/bin/env bash
set -e

version=$(date +"%y.%m.%d.%H.%M")

docker login -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"

docker build -t leonti/rea-frontend:$version .
docker push leonti/rea-frontend:$version

echo $version" is built"
