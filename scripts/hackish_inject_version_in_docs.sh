#!/usr/bin/env bash
set -ex

VERSION=$(./scripts/version_from_git.sh)
perl -pi -e "s/^\@version .+$/\@version ${VERSION}/g" overview.edoc
