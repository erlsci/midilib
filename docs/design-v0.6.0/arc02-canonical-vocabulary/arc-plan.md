# Arc 2 — Canonical vocabulary · arc plan

**Release:** v0.6.0 · **Arc:** 2 of 6 · **Depends on:** Arc 1 · **Status:** in progress
**Design of record:** [`DESIGN-vocabulary.md`](./DESIGN-vocabulary.md)

## Goal

Land the canonical message vocabulary and the error vocabulary as **foundation
only** — the artifacts arcs 3–5 will adopt — without rewiring any codec yet, so
the build and the existing `midibin` tests keep passing throughout.

## Slices

### Slice 1 — Vocabulary design doc ✅
`DESIGN-vocabulary.md`. The message()/event() split, the full record catalogue,
the tag-name migration map, the error contract, alternatives, and the
consumer-coordination notes. *Done (this arc's design slice — the unification
brainstorm).*

### Slice 2 — Vocabulary + error foundation
- `include/midi_msg.hrl` — all records from §4 + the `-type` unions
  (`message()`, `event()`, field types). Inert until included.
- `midierrs.erl` — `reason()` type + `format_error/1` for every reason (§6).
  Closes #24 (dead module gets a job) and stages the fix for #16.
- `rebar.config` — reconcile the coverage alias: drop the hardcoded
  `--min_coverage=0` so the misleading always-pass gate is gone; enforcement is
  Makefile/CI-owned via `COVERAGE_MIN` (Arc 1). Closes CDC miss **M4** at the
  rebar layer.

## Explicitly NOT in this arc (deferred to named arcs)
- Rewiring `midibin` to records + `{ok,_}|{error,_}` and migrating its tests —
  **Arc 3**.
- Rewiring `midifile` (events, `#seq{}`, SMPTE division) — **Arc 4**.
- Rewiring `midimsg`/`midiutil` constructors + the `tempo_bpm`→`#meta_set_tempo{}`
  convenience + LFE-accessor question — **Arc 5**.
- Removing the legacy `include/errors.hrl` string macros — happens with the
  `midibin` migration in **Arc 3** (removing them now breaks current tests).

## Acceptance criteria (ledger steps)

1. `include/midi_msg.hrl` defines every record and union type in DESIGN §4, with
   field-level `-type`s; no record/type name collides with `include/midi.hrl`.
2. A throwaway module that `-include("include/midi_msg.hrl")` and constructs one
   of each record compiles clean under `rebar3` (validation harness; not shipped)
   — to be run when an Erlang toolchain is available (see Validation note).
3. `midierrs:format_error/1` is total over `reason()` (a clause per reason
   constructor) and returns a binary/string; `-export_type([reason/0])`.
4. `rebar.config` coverage alias no longer hardcodes `--min_coverage=0`;
   `make coverage COVERAGE_MIN=N` remains the single enforcement point.
5. `make build` / existing `midibin` tests still pass (nothing rewired yet).

## Validation note (honesty / capability)

The authoring environment has no Erlang toolchain, so the `.hrl`/`.erl` here are
authored against the language spec and the existing source's idioms but are
**not compile-checked in place**. Criterion 2/3/5 are verified when the arc is
run on a machine with `rebar3` (or fold into Arc 3's first compile, which
includes the header). This gap is named, not hidden.
