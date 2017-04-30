#!/usr/bin/env bash

# sigh.....
rebar3 as generate_documentation compile
mkdir -p _build/generate_documentation/lib/taskforce/doc/
cp -p overview.edoc _build/generate_documentation/lib/taskforce/doc/
erl -pa _build/generate_documentation/lib/*/ebin -noshell -run edoc_run application "taskforce"
erl -pa _build/generate_documentation/lib/*/ebin -noshell -run edoc_run application "taskforce" '[{doclet, edown_doclet}, {top_level_readme, {"README.md", "https://github.com/g-andrade/taskforce", "master"}}]'
rm -rf doc
mv _build/generate_documentation/lib/taskforce/doc ./
sed -i -e 's/^\(---------\)$/\n\1/g' README.md
