#!/usr/bin/env bash
set -ex

GIT_HEAD=$(cat .git/HEAD)
TAG_DESCRIPTION=$(git describe --tags)
./scripts/version_from_git.py "$GIT_HEAD" "$TAG_DESCRIPTION"
