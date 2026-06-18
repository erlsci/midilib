# CC assignment — Arc 3: `midibin` onto the canonical vocabulary

> The assignment the implementing context (CC) receives. Self-contained. Read the
> reference docs first, then implement to the ledger. **A CDC-authored draft of
> the implementation already exists in the tree** (see §Draft) — treat it as a
> reviewed starting point to validate, finalize, and *own*, not as gospel. CDC
> verifies independently on close.

## Posture

Peer-frame, write-to-the-floor. Load the **collaboration-framework** and
**erlang-guidelines** skills (`11-anti-patterns.md` first; also `02-api-design.md`,
`04-data-and-types.md`, `15-testing.md`). The canonical vocabulary is the
contract — do not invent message shapes; use the records in
`include/midi_msg.hrl` verbatim.

This is the first module to adopt the v0.6.0 vocabulary. It is deliberately the
small, clean, already-tested module, so it is where the new types get proven
before `midifile` (Arc 4) leans on them.

## Required reading (evidence, not summaries)

1. `docs/design-v0.6.0/arc02-canonical-vocabulary/DESIGN-vocabulary.md` — the
   message()/event() split, the record catalogue (§4), the error contract (§6),
   and the normalization stance (§3.5).
2. `docs/design-v0.6.0/arc03-midibin/arc-plan.md` — the new public API, the
   findings closed, and the decisions (range guards, C5 policy, clause ordering).
3. `docs/design-v0.6.0/arc03-midibin/ledger.md` — the 13 acceptance rows you
   close with evidence.
4. `include/midi_msg.hrl` and `src/midierrs.erl` — the vocabulary and error
   reasons (these are settled "our-side" artifacts; use them, don't redefine).
5. Audit context for the findings this arc closes:
   `docs/design-v0.6.0/arc00-audit/2026.06.17-audit-results-erlang.md` (#8, #11,
   #14, #16, #20, #21), `…-consumer-fit.md` (C1, C3, C4, C5), and the CDC eval
   `…-audit-eval-cdc.md`.

## Draft to validate and finalize (do not blindly adopt)

A CDC draft is already in the tree. It was authored **without an Erlang
toolchain**, so it is unverified — it has *not* been compiled, eunit-run, or
PropEr-run. Your job is to make it real:

- `src/midibin.erl` — full rewrite onto records + `{ok,_}|{error,_}`, multi-byte
  SysEx, channel-mode incl. `mono_mode_on`, list-based `encode_batch/1` /
  `decode_batch/1`, range-guarded encode, `mode_control/2` helper.
- `test/midibin_tests.erl` — record-based eunit round-trips, the multi-byte SysEx
  test, batch tests, error-contract tests.
- `test/midibin_props.erl` — PropEr generators + `prop_roundtrip` /
  `prop_encode_starts_with_status_byte`.

**Specifically re-derive, don't trust:** every bit-syntax pattern (status nibbles,
14-bit LSB/MSB order, the `<<16#F0, …, 16#F7>>` framing and the `binary:part/at`
SysEx split), the channel `±1` mapping, and the PropEr generators (esp. that
`control()` is restricted to 0..119 so the property doesn't collide with the
channel-mode decode). If the draft is wrong, fix it — you own the result, and CDC
will check it against the design, not against the draft.

## OTP floor — a decision you must surface, not make alone

The draft uses **no** OTP-27-only syntax (the CDC draft caught and removed its own
`-doc`/`-moduledoc` + triple-quoted strings, because `README.md` advertises OTP
**21–27**). Two paths; **ask the maintainer which**, and apply it as ledger row 12:

- **Keep portable (≥ stated floor):** no `-doc`/`-moduledoc`; EDoc/`%%` comments
  only. (Draft is already in this state.)
- **Bump to OTP 27+ for v0.6.0:** restore `-doc`/`-moduledoc` (the
  erlang-guidelines default), and update `README.md` badge + `.app.src` +
  CI/`rebar.config` to declare the new floor.

Do not bump the floor unilaterally.

## Constraints (erlang-guidelines)

- snake_case; `-spec` every exported function using `message()` / `midierrs:reason()`
  (no `-> tuple()`); `{ok,_}`/`{error,_}` returns.
- Let-it-crash for genuinely malformed input, but **no silent truncation** —
  encode must reject out-of-range fields via the guards rather than letting a
  `:7` segment wrap (the draft's `?is_chan`/`?is_u7`/`?is_u14` guards).
- One complete message per `decode/1` — no running-status expansion, no stream
  reassembly (NEEDS R1).
- Clause ordering (FP-12): specific channel-mode clauses before the generic CC
  clause; the audit's clean-check #5 confirmed the old ordering was sound — keep
  that discipline and confirm no clause is shadowed.
- Velocity-0 note-on is **not** folded to note-off (C8/R6) — preserve it.

## Out of scope (do not build)

`midifile`, `midimsg`, `midiutil` changes; the `#event{}`/`#seq{}` containers;
removing `include/errors.hrl` (the maintainer is handling that deletion); the
OTP-floor *decision* (surface it). Those are other arcs / other owners.

## Done

Every ledger row closed with cited evidence, and `rebar3 check` green
(compile + xref + dialyzer + eunit + coverage) plus `rebar3 proper` green, on the
agreed OTP floor. Write `closing-report.md` (per-row walk with evidence). If a row
can't be met, stop and surface it (disclosed deferral) — don't silently drop it.
Five-iteration cap. CDC then writes `cdc-verification.md` independently.
