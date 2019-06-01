#!/bin/bash

set -e

tag=$1

export TRAVIS_BUILD_RELEASE_TAG="${tag}"
release-manager \
    --config "./.travis/release.yml" \
    --check-version \
    --make-version \
    --make-artifact \
    --upload-artifact
