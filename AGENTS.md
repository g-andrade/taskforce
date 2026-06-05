# taskforce

Erlang/OTP library for parallelising arbitrary tasks in a controlled way. It
runs a group of independent tasks concurrently, bounding both the number of
workers and how long execution may take (per-task and global timeouts), and
gathers their results. Pure library, no runtime dependencies.

## Build, test, check

```bash
make compile         # compile
make test            # eunit + coverage
make check           # check-fast + check-slow
make check-fast      # format check (erlfmt) + xref + dead-code (hank) + lint (elvis)
make check-slow      # dialyzer
make format          # auto-format source with erlfmt
make eunit           # unit tests + coverage
make dialyzer        # type analysis
make doc             # EEP-48 chunks (rebar3 edoc) + ex_doc HTML into doc/
make shell           # interactive REPL with the app started
```

All checks run sequentially (`.NOTPARALLEL`). CI runs `make check-fast`,
`make test`, and `make check-slow` on OTP 24–29 (Linux). There are no Common
Test suites; tests are eunit only.

## Compiler flags

Always on: `warn_export_vars`, `warn_missing_spec`, `warn_unused_import`,
`warnings_as_errors`. Every exported function must have a `-spec`. The `test`
and `shell` profiles relax `warn_missing_spec` and `warnings_as_errors`. The
`E48` macro (`platform_define`, OTP 27+) guards the EEP-48 doc attributes.

## Architecture

Each `taskforce:execute/1,2` call spins up a short-lived master/minion tree:

```
taskforce_sup                      (one_for_one)
├── tf_master_sup                  (simple_one_for_one, temporary workers)
│   └── tf_master_serv             (one gen_server per execute/1,2 call)
└── tf_minion_sup                  (simple_one_for_one, transient workers)
    └── tf_minion_serv             (N gen_servers, the workers)
```

- `execute/1,2` starts one `tf_master_serv` (the *master*) and sends it the
  shuffled task list with a global timeout.
- The master spawns up to `max_workers` `tf_minion_serv` *minions*. Each minion
  pulls a task from the master (`consume_task`), runs it under its individual
  timeout, and casts the result (or timeout) back.
- The master replies to the original caller once every task is accounted for
  (completed or timed-out), or when the global timeout fires, then terminates.
  Minions and master monitor each other and their patron, so a death anywhere
  tears the execution down cleanly.

### Key modules

| Module | Role |
|---|---|
| `taskforce` | Public API: `task/3`, `execute/1`, `execute/2` (plus deprecated `new_task/4`, `execute_tasks/1-3`) |
| `taskforce_app` | `application` callback |
| `taskforce_sup` | Top supervisor |
| `tf_master_sup` | `simple_one_for_one` supervisor of masters |
| `tf_master_serv` | Master gen_server: hands tasks to minions, collects results, enforces the global timeout |
| `tf_minion_sup` | `simple_one_for_one` supervisor of minions |
| `tf_minion_serv` | Worker gen_server: pulls a task, runs it under its individual timeout, reports back |

Shared records and types (`tf_task`, `tf_bidding`, `tf_bidding_result`, …) live
in `include/taskforce.hrl`, included as `-include("taskforce.hrl")`.

## Code conventions

- Code is formatted with `erlfmt`; run `make format` before committing. The
  reformat commit is listed in `.git-blame-ignore-revs`.
- **Documentation is EEP-48 native** (`-moduledoc`/`-doc`), guarded by
  `-ifdef(E48)`. Private modules and functions are hidden with
  `-moduledoc false` / `-doc false` — **not** `@private`, which ex_doc ignores.
  Only `taskforce` is a public module; the rest are `-moduledoc false`.
- Docs build via `rebar3 edoc` (top-level `edoc_opts`: chunks doclet/layout,
  `{preprocess, true}`, output under `_build/docs/lib/taskforce/`), then the
  pinned `ex_doc` escript renders HTML. `make doc` runs both. There is **no**
  `docs` rebar3 profile and the pipeline does **not** use `rebar3 as docs`.
- Internal functions only callable by tests are tagged `-ignore_xref([…])`.
- `elvis.config` test rules relax `no_debug_call`,
  `no_receive_without_timeout`, and `dont_repeat_yourself`.

## Tests

`test/taskforce_test.erl` (eunit, guarded by `-ifdef(TEST)`) exercises the full
execute pipeline by calculating prime numbers in parallel, over both the current
and the deprecated APIs. `examples/` holds a standalone usage example.

## OTP version notes

`rebar.config.script` strips dev plugins on older OTP: `erlfmt` is dropped on
OTP ≤ 26 (it chokes on `-doc` triple-quoted strings via katana_code), `erlfmt` +
`rebar3_hank` + `rebar3_lint` on OTP ≤ 25, and `rebar3_hank` on OTP 29 (a
katana_code/hank bug). These paths only exercise on CI's older runners.

## Releasing

`make publish` runs `rebar3 hex publish --doc-dir=doc`. Versioning follows
SemVer; history is in `CHANGELOG.md` (Keep a Changelog format).
