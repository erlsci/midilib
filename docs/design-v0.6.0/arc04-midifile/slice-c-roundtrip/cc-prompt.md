# CC assignment — Arc 4 · Slice C: round-trip property + coverage + error paths

> Self-contained. The slice-A reader, `midi_codec`, and the slice-B writer are
> landed and CDC-verified. This slice proves the codec with a property, raises the
> coverage floor, and completes the reader's error contract. It **closes Arc 4**.
> CDC verifies independently on close.

## Posture

Peer-frame, write-to-the-floor. Load the **collaboration-framework** and
**erlang-guidelines** skills (`11-anti-patterns.md` first; also `15-testing.md` for
PropEr, `03-error-handling.md`, `17-tooling.md` for the coverage gate). Use the
canonical records (`include/midi_msg.hrl`) and `midierrs:reason()` verbatim.

**On "let it crash":** crashing is for the *unexpected* (bugs, broken internal
invariants). Foreseeable bad *external* data — a truncated or corrupt file — is an
`{error, _}` **value**. This slice finishes applying that line to the reader (see
§ corruption-as-value), the same principle that fixed F-CDC-1.

## Required reading

1. `docs/design-v0.6.0/arc04-midifile/arc-plan.md` and this slice's
   `slice-c-roundtrip/arc-plan.md` — the generator design, the coverage plan, the
   corruption-as-value decision, the arc closeout.
2. `docs/design-v0.6.0/arc04-midifile/slice-c-roundtrip/ledger.md` — the rows.
3. `src/midifile.erl` (reader `parse/2` + writer `to_binary/1`/`write/2`),
   `src/midi_codec.erl`, `src/midierrs.erl`, `include/midi_msg.hrl`.
4. `test/prop_midibin.erl` — the Arc-3 PropEr style and the `control() ∈ 0..119`
   restriction (the same C5-collision issue applies to your generator).
5. The slice-A/B `cdc-verification.md` + `closing-report.md` (the F-CDC-1 lesson,
   the coverage-measurement gap, N-CDC-1).
6. `rebar.config` — the `coverage`/`check` aliases and the `--min_coverage`
   setting you are fixing (M4).
7. Audit/CDC context: `…-results-erlang.md` (#19, #20, #22 coverage), the CDC eval
   `…-audit-eval-cdc.md` (M4; the round-trip recommendation).

## What to build

1. **`test/prop_midifile.erl`** — `prop_roundtrip`:
   `from_binary(to_binary(Seq)) =:= {ok, Seq}` over a `seq()` generator. Build the
   generator to the **round-trippable, file-domain** constraints in the slice plan
   — these are correctness, not taste: `control_change` control ∈ 0..119;
   `channel_mode` value 0 except `mono_mode_on`; time-sig denominator a power of
   two; text payloads **binary**; `meta_unknown` type ∈ unmodelled-only; sysex
   unrestricted; **no system-common/real-time messages**; tracks end in
   `#meta_end_of_track{}`; delta ∈ 0..16#0FFFFFFF.
2. **`src/midifile.erl`** — add pure **`from_binary/1`** (read from an in-memory
   binary; `read/1` becomes `file:read_file/1` + `from_binary/1`). Promote the
   reader's foreseeable-corruption crash paths to `{error, midierrs:reason()}`
   (see § corruption-as-value).
3. **`src/midierrs.erl`** — add the new reasons + `format_error/1` clauses for the
   promoted cases. **Do not touch its `-doc`/`-moduledoc` attributes** (Arc 6).
4. **`rebar.config`** — fix the coverage measurement (aggregate eunit + proper
   cover data) and raise the floor from 0 to the agreed threshold; wire it so
   `make check` / CI enforces it.
5. **Tests** — negative-path tests for each promoted reason (now `{error,_}`, not
   `?assertError`); explicit `read(write(Seq)) =:= Seq` fixtures for **format 0, 1,
   2**. **Update the slice-A `truncated_chunk_still_crashes_test`** to the new
   `{error,_}` contract and rename it — and **disclose this** in the closing
   report (it is a deliberate contract change, not a quiet test edit).

## Corruption-as-value (contract decision — confirm with the maintainer first)

This evolves the slice-A reader contract (verified there as "crash on malformed").
Recommended: promote foreseeable file corruption to values — `{truncated, _}`,
`{missing_tracks, Got, Want}`, `{bad_division, Word}`, `{bad_running_status, _}` —
so `read/1`/`from_binary/1` crash **only** on internal invariant violations.
**If the maintainer declines**, ship the property + coverage + negatives asserting
the *current* crash behaviour instead, and say so. Do not decide this unilaterally.

## Specifically re-derive, don't trust

The generator's per-type round-trip restrictions (test each by reasoning: would
`from_binary(to_binary(M)) =:= {ok, M}` hold for every value the generator can
emit?), and the coverage-aggregation wiring (confirm the reported number actually
includes eunit by checking an eunit-only line shows as covered).

## Constraints (erlang-guidelines)

- snake_case; `-spec` every new export (`from_binary/1`); `{ok,_}`/`{error,_}`.
- **No process dictionary**; the reader/writer stay state-threaded/stateless.
- New errors are **structured `midierrs:reason()` values**, never strings, never
  `exit/1` (EH-12). Human text only via `format_error/1`.
- **Portable (OTP 22–29):** `%%` + `-spec`, no `-doc`/`-moduledoc` in the modules
  you touch; leave `midierrs`'s existing `-doc` lines untouched.
- Don't change `include/midi_msg.hrl` or the canonical records. If a record can't
  round-trip, that's a generator restriction or a *surfaced* design question — not
  a header edit.

## Out of scope

`midimsg`/`midiutil` (Arc 5); the `midierrs` `-doc` reconciliation (Arc 6);
running-status compression (deferred). Don't re-open slices A/B beyond the
disclosed reader-contract change and the coverage wiring.

## Done

Every ledger row closed with cited evidence; `prop_roundtrip` green at the agreed
numtests; `rebar3 as test check` green with the **raised** coverage floor enforced;
all prior suites (A reader, B writer, Arc-3 `midibin`) still green. Write
`closing-report.md` including the **arc-level reconciliation** (every Arc-4 finding
shown closed in A/B/C — the release-plan "no silent drops" gate). If a row can't be
met, stop and surface it. Five-iteration cap. CDC then writes `cdc-verification.md`
and the Arc-4 close.
