#!/bin/bash

set -e

tag=$1

project_version=$(sbt version -Dsbt.log.noformat=true | tail -n 1 | perl -ne 'print $1 if /(\d+\.\d+[^\r\n]*)/')

cd "${TRAVIS_BUILD_DIR}"

if [ "${project_version}" == "${tag}" ]; then
    export TRAVIS_BUILD_RELEASE_TAG="${release}"
    release-manager \
        --config "./.travis/release.yml" \
        --check-version \
        --make-version \
        --make-artifact \
        --upload-artifact
else
    echo "Tag version '${tag}' doesn't match version in scala project ('${project_version}'). Aborting!"
    exit 1
fi
