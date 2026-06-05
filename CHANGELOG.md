# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- support for OTP 26, 27, 28 and 29
- `ex_doc`-based documentation with EEP-48 (`-moduledoc`/`-doc`) attributes
- dev tooling: `erlfmt`, `rebar3_hank` and `elvis` (via `rebar3_lint`)
- specs for all exported functions
### Changed
- CI to GitHub Actions with an OTP 24-29 matrix (replacing the container build)
- build system to the current rebar3-based Makefile / `rebar.config`
- supervisor child specifications to the map-based form
### Removed
- support for OTP older than 24
- pre-OTP-18/19 conditional code paths
- the legacy edoc/edown documentation tooling and tracked `doc/` output
- the semi-archival/maintenance notice (the library is maintained again)

## [1.2.3] - 2019-01-19
## [1.2.2] - 2017-09-03
## [1.2.1] - 2017-04-30
## [1.2.0] - 2017-04-30
## [1.1.0] - 2017-04-30
## [1.0.3] - 2017-04-30
## [1.0.2] - 2017-04-30
## [1.0.1] - 2016-10-17
## [1.0.0] - 2015-04-03
