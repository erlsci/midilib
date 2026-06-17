# Arc 1 — Build system & developer workflow

**Release:** v0.6.0 · **Arc:** 1 of 6 · **Depends on:** nothing · **Status:** in progress

## Goal

Give midilib a colourful, self-documenting `make` front end in the house style
(modelled on `workbench/Makefile` + `workbench/mk/lfe.mk`), so every later arc
runs through one consistent quality gate. This arc is **pure tooling** — it adds
no behaviour to `src/` and changes no `.erl`. It is first because it's
foundational, isolated, low-risk, and reversible, and because it lets us set a
**real, rising coverage floor** instead of the audit's `--min_coverage=0` (CDC
miss **M4**).

## Shape

midilib is single-language Erlang/rebar3, so the polyglot LFE+Rust split of the
example collapses to one module:

```
Makefile          ← shared vars (colours, identity, git), aggregate targets, help-general, info, check-tools
mk/erlang.mk      ← Erlang/BEAM targets, all suffixed -erl, help-erl
```

Run everything through `Makefile`, never `mk/erlang.mk` directly.

## Targets

**Aggregate (top Makefile):** `help` (`.DEFAULT_GOAL`), `build`, `test`,
`lint`, `format`, `coverage`, `docs`, `check`, `ci`, `clean`, `info`,
`check-tools`.

**Erlang module (`-erl` suffix):** `compile-erl`, `test-erl` (eunit + ct),
`test-unit-erl`, `test-ct-erl`, `test-proper-erl`, `lint-erl` (xref),
`xref-erl`, `dialyzer-erl`, `coverage-erl`, `docs-erl`, `format-erl`,
`shell-erl`, `clean-erl`, `distclean-erl`.

**Style requirements (from the example):** ANSI colour vars (`BLUE/GREEN/
YELLOW/RED/CYAN/RESET`); boxed `help-general` banner as the default goal;
coloured section headings and `✓`/`✗` status lines; identity vars
(`PROJECT_NAME=midilib`, `APP_VERSION` from `src/midilib.app.src`, `GIT_BRANCH/
COMMIT`, `OTP_VERSION`, `REBAR=rebar3`); graceful degradation when an optional
tool is absent (erlfmt, ex_doc), mirroring the example's `→ … skipping` pattern.

## Design choices (decided here)

- **Coverage floor is Makefile-owned and overridable.** `COVERAGE_MIN ?= 0`
  today (only `midibin` is tested; a higher floor would fail honestly). The
  `coverage-erl` target invokes cover with `--min_coverage=$(COVERAGE_MIN)`,
  bypassing the hardcoded-0 alias in `rebar.config`. Each later arc raises the
  default as its tests land; the rebar.config alias is reconciled in Arc 2.
- **`dialyzer-erl` is available but out of the default `lint` gate** initially
  (it's clean-ish here but kept deliberate), matching the example's stance.
- **`test/fixtures/` is created** (with a `.gitkeep`) so Arc 4's round-trip
  tests have a home.
- No `rebar.config` edits in this arc (keeps it pure-tooling); the
  `--min_coverage` reconciliation is tracked for Arc 2.

## Acceptance criteria (ledger steps)

1. `Makefile` and `mk/erlang.mk` exist at repo root; `make` with no args prints
   the boxed help (general + erlang sections) and exits 0.
2. `make info` and `make check-tools` render the coloured blocks and exit 0 even
   with no Erlang toolchain present (tools show "not installed", no crash).
3. `make -n check` / `-n build` / `-n test` expand to the expected rebar3
   command chains with no unresolved variables or missing targets.
4. `APP_VERSION` resolves from `src/midilib.app.src` (0.5.2 now).
5. `COVERAGE_MIN` is overridable: `make coverage-erl COVERAGE_MIN=80` threads the
   value into the cover invocation (verified via `make -n`).
6. No file under `src/`, `include/`, or `test/*.erl` is modified by this arc.

## Out of scope (this arc)
- Raising the actual coverage floor (no tests to support it yet — Arcs 3–5).
- Adding erlfmt / ex_doc plugins to `rebar.config` (targets degrade gracefully
  until then; plugin wiring rides Arc 6 docs/format work).
- Any `src/` change.
