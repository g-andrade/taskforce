# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [1.3.0] - 2026-06-06

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

### Changed

- pinned the imported `rebar3_hex` plugin to a stable version
- Hex package metadata to link to the GitLab mirror
- regenerated documentation

## [1.2.2] - 2017-09-03

### Fixed

- typo in the documentation overview

## [1.2.1] - 2017-04-30

### Changed

- made the documentation more precise

## [1.2.0] - 2017-04-30

### Added

- map-based interface: `task/3`, `execute/1` and `execute/2`

### Changed

- internal modules are now hidden from the documentation

### Deprecated

- the list-based functions `new_task/4` and `execute_tasks/1,2,3`

## [1.1.0] - 2017-04-30

### Added

- exported the application-defined types
- a basic test case and a usage example

### Changed

- updated the application description

### Removed

- the `crypto` dependency (it was overkill)

### Fixed

- missing startup module definition in the `.app` file
- empty module list in the `.app` file
- Dialyzer complaints

## [1.0.3] - 2017-04-30

### Fixed

- missing startup module definition in the `.app` file (hotfix on the 1.0.x line)

## [1.0.2] - 2017-04-30

### Changed

- migrated the build to rebar3
- updated the application description and properties

## [1.0.1] - 2016-10-17

### Changed

- switched from the edown fork to upstream edown

### Fixed

- documentation generation under Erlang/OTP 17.5

## [1.0.0] - 2015-04-03

### Added

- initial release: on-demand worker pools for parallelizable tasks
- the list-based API: `new_task/4` and `execute_tasks/1,2,3`, bounding the
  number of workers and enforcing per-task and global timeouts
