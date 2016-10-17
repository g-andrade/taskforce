#!/usr/bin/env bash

# sigh.....
sed -i -e 's/^\(\s*\)%{edown/\1{edown/g' -e 's/^\(\s*\)%{doclet, edown/\1{doclet, edown/g' rebar.config
rebar get-deps
rebar compile
pushd doc
erl -pa ../deps/*/ebin -pz ../ebin -noshell -run edoc_run packages '[""]' '[{source_path, ["../src"]}]'
erl -pa ../deps/*/ebin -pz ../ebin -noshell -run edoc_run packages '[""]' '[{source_path, ["../src"]}, {doclet, edown_doclet}, {top_level_readme, {"../README.md", "https://github.com/g-andrade/taskforce", "master"}}]'
popd
sed -i -e 's/^\(\s*\){edown/\1%{edown/g' -e 's/^\(\s*\){doclet, edown/\1%{doclet, edown/g' rebar.config
sed -i -e 's/^\(---------\)$/\n\1/g' README.md
