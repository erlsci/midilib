# CC assignment — Arc 4 · Slice B: `midifile:write/2`

> Self-contained. The slice-A reader + `midi_codec` are landed and CDC-verified;
> you build the writer on top of them. CDC verifies independently on close.

## Posture

Peer-frame, write-to-the-floor. Load the **collaboration-framework** and
**erlang-guidelines** skills (`11-anti-patterns.md` first; also `03-error-handling.md`,
`04-data-and-types.md`, `05-functions-and-pattern-matching.md`, `15-testing.md`).
Use the canonical records (`include/midi_msg.hrl`) and `midierrs:reason()` verbatim.

**On "let it crash" (this bit us in slice A — internalise it):** crashing is for
the *unexpected* — programmer errors and genuinely-malformed internal state. It is
**not** for foreseeable bad external data. A record with an out-of-range field
handed to the writer is foreseeable bad input → it is an `{error, _}` **value**,
not a crash. (The symmetric reader question — corrupt files — is slice C's.)

## Required reading (evidence, not summaries)

1. `docs/design-v0.6.0/arc04-midifile/arc-plan.md` — the three-layer architecture
   and the vocabulary/error contract.
2. `docs/design-v0.6.0/arc04-midifile/slice-b-writer/arc-plan.md` — this slice's
   design: the writer structure, explicit-status decision, SysEx framing, the
   N-CDC-1 error stance.
3. `docs/design-v0.6.0/arc04-midifile/slice-b-writer/ledger.md` — the rows to close.
4. `src/midifile.erl` — the slice-A reader you are mirroring (`parse/2`,
   `decode_division/2`, `parse_sysex/1`, `read_vlq/1`); your writer is its inverse
   and must round-trip against it.
5. `src/midi_codec.erl` — `encode_message/1`, `encode_meta/1`, `data_length/1`;
   the encode side you extend for N-CDC-1.
6. `docs/design-v0.6.0/arc02-canonical-vocabulary/DESIGN-vocabulary.md` §4–§6, §9
   — records, tag set, error reasons, the `#seq{}`/division/conductor decisions.
7. The slice-A `cdc-verification.md` (the F-CDC-1 lesson and N-CDC-1 statement).
8. Audit context: `…-results-erlang.md` (#2, #3, #5, #6, #7, #17, #18),
   `…-midi-spec.md` (S3, S5), `…-consumer-fit.md` (C7).

## What to build

1. **`src/midifile.erl` `write/2`** + a pure **`to_binary/1`** it delegates to
   (so the byte assembly is filesystem-free and slice C can reuse it). Header with
   the **real format** and `length(tracks)` (#6/S5); division as the inverse of
   `decode_division/2`; `MTrk` chunks with correct `Len`; per-event `vlq(Delta)` +
   message bytes via `midi_codec`; SysEx framed `F0 vlq(byte_size(D)+1) D F7`.
   **Explicit status byte on every channel event — no running-status compression**
   (see slice plan for why; this kills #7 and the writer half of #18 structurally).
   Delete the stub.
2. **`src/midi_codec.erl`** — extend the encode side for the **N-CDC-1** uniform
   stance: every modelled `encode_meta/1` record validates its fields and returns
   `{error, {bad_value, Field, Value}}` on a range violation (not `function_clause`);
   align `encode_message/1` to return `{bad_value, Field, _}` for a merely
   out-of-range field. Additive to its tests.
3. **`test/midifile_tests.erl`** — writer fixtures: write a hand-built `#seq{}` and
   re-read it through the slice-A reader to an **equal** `#seq{}` (the round-trip
   proof of #2/#3/#6 for fixed inputs); format 0/1/2 header bytes; SysEx framing
   incl. a payload ending in `F7`; note-off velocity preserved; oversized delta and
   out-of-range fields → `{error, {bad_value, _, _}}`.
4. **`test/midi_codec_tests.erl`** — the new `{bad_value, _}` encode cases.

## Specifically re-derive, don't trust

The division *encode* (two's-complement SMPTE high byte), the `MTrk` length
computation, the SysEx length-includes-`F7` framing, and the VLQ writer's
`0..16#0FFFFFFF` bound. Verify each by writing then reading back through the
slice-A reader — byte-identity with the reader's expectations is the test.

## Constraints (erlang-guidelines)

- snake_case; `-spec` every export; `{ok,_}`/`ok`/`{error,_}` returns.
- **No process dictionary** anywhere in the writer (#18). The writer carries no
  cross-event state (no running status), so this is automatic — keep it that way.
- **No silent truncation, no `exit/1`-on-a-string** (#5/#17). Out-of-range →
  `{error, {bad_value, Field, Value}}`. Build the whole binary in memory and only
  then `file:write_file/2`, so a failure leaves **no partial file**.
- **Faithful writer:** write exactly the events given — no auto-append/strip of
  End-of-Track. Document the "each track ends with `#meta_end_of_track{}`"
  invariant.
- **Portable (OTP 22–29):** `%%` + `-spec`, no `-doc`/`-moduledoc`. Do not touch
  `midierrs` (its `-doc` is Arc 6) or `include/*.hrl`. If you think a vocabulary
  record or reason is wrong, **stop and surface it** — don't edit it.
- Clause ordering (FP-12); confirm no shadowing in the new `encode_*` clauses.

## Out of scope (do not build)

The round-trip PropEr property, the coverage floor, and the **reader
corruption-as-value** change (truncated chunk / over-declared `ntrks` / zero
division → `{error,_}`) — all **slice C**. Do not modify the slice-A reader's
crash behaviour here. No `midimsg`/`midiutil` (Arc 5).

## Done

Every ledger row closed with cited evidence; `rebar3 as test check` green and
`rebar3 proper` green on the OTP floor; the **slice-A `midifile` reader suite and
the Arc-3 `midibin` suite stay green** (re-confirm — your `midi_codec` encode
changes must not regress either). Write `closing-report.md` (per-row walk, the
decisions you own, any disclosed deferral). If a row can't be met, stop and
surface it. Five-iteration cap. CDC then writes `cdc-verification.md`.
