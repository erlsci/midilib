# Arc 4 — `midifile` rewrite (state-threaded, round-trip-tested) · arc plan

**Release:** v0.6.0 · **Arc:** 4 of 6 · **Depends on:** Arc 2, Arc 3 · **Status:** planning
**Design of record:** [`../arc02-canonical-vocabulary/DESIGN-vocabulary.md`](../arc02-canonical-vocabulary/DESIGN-vocabulary.md)
**Drives from:** the CC audit (`../arc00-audit/2026.06.17-audit-results-erlang.md`,
`…-midi-spec.md`, `…-consumer-fit.md`) and the CDC evaluation
(`…-audit-eval-cdc.md`, verdict GO; trend cluster B/C → "treat `midifile` as a
state-threaded rewrite, not spot fixes").

## Goal

Rewrite `midifile` from the 2010-vintage, process-dictionary, three-tuple module
into a state-threaded SMF codec that speaks the **canonical vocabulary**
(`include/midi_msg.hrl`) and the `{ok,_} | {error, reason()}` contract, proven by
a `read(write(Seq)) =:= Seq` PropEr property over generated sequences.

This is the arc where **every strict Blocker lives** (#1, #2, #3 are all in
`midifile`; #8 was the fourth and closed in Arc 3). The CDC evaluation is explicit
that spot-fixing #1–#4 "leaves the process-dict and vocabulary debt that produced
them" — so this is a rewrite, and the round-trip property is the test that locks
the Blockers shut and exposes the next layer.

## The keystone architecture decision: three layers

The audit's cluster A (vocabulary split) and cluster B (process-dict state) are
both dissolved by separating *message↔bytes* from *framing*. We introduce a third
module so the canonical-message codec has **one home**, and the two framers sit on
top of it:

```
            ┌──────────────────────────────────────────────┐
            │  midi_codec   (new, internal, pure)           │
            │  canonical message() <-> {status, data-bytes} │
            │  no I/O · no process state · no framing        │
            └──────────────────────────────────────────────┘
                     ▲                          ▲
        wire framing │                          │ file framing
        (complete    │                          │ (running status,
         messages)   │                          │  VLQ delta-times,
                     │                          │  MThd/MTrk chunks,
            ┌────────┴───────┐         ┌────────┴────────────┐
            │   midibin      │         │     midifile        │
            │ (wire codec)   │         │  (SMF read/write)   │
            └────────────────┘         └─────────────────────┘
```

- **`midi_codec`** — pure, stateless, framing-free. It maps a canonical
  `message()` to/from its raw status byte + data bytes (and meta records to/from
  `{type_byte, payload_binary}`). It has no concept of running status, delta-time,
  files, or wire transport. This is the single source of truth for the
  channel/system/meta byte layout, so the two framers can never drift in how a
  note-on is laid out (the exact drift the audit found between read and write).
- **`midibin`** stays the *wire* framer: one complete message per call. It is
  refactored to consume `midi_codec` for the body mapping (its public API and its
  green Arc-3 suite are unchanged — a behaviour-preserving extraction).
- **`midifile`** is the *file* framer: chunk structure, the variable-length
  quantity codec, running-status elision/expansion, and the per-track state
  threading — all built on `midi_codec` for the message bodies and on
  `midierrs:reason()` for failures.

Why a new module rather than `midifile` calling `midibin`: `midibin` works on
*complete framed* binaries, while `midifile` needs the body mapping *without* the
wire framing (it supplies its own delta-time and running-status framing). Routing
`midifile` through `midibin` would force re-framing round-trips; routing both
through a framing-free core is clean. (This is the "extract a shared internal
codec module" decision, accepted at planning time. It is an intentional,
disclosed re-touch of the just-closed `midibin` — behaviour-preserving, guarded by
midibin's existing suite plus the new round-trip property.)

## Slice breakdown

Arc 4 splits into three slices. Dependencies are load-bearing.

| Slice | Name | Depends on | Closes |
|------:|------|------------|--------|
| **4a** | **`midi_codec` + `midibin` refactor + `midifile:read/1`** | Arc 3 | #1/S2, #4/S4, S7, #15, #13/C7 (read side), #7/S8/C8 (read side), M1 (read side), #18 (read side), #22, #26, M2; #19 (reader tests + fixtures) |
| **4b** | **`midifile:write/2`** | 4a | #2/S3, #3, #5/#17, #6/S5, #13/C7 (write side), #7 (write side), M1 (write side), #18 (write side); #19 (writer tests) |
| **4c** | **round-trip property + coverage + error paths** | 4a, 4b | #19 (the `read(write(Seq))=:=Seq` PropEr property over generated `#seq{}`), the non-zero coverage floor, S7/M2 negative-path tests, format 0/1/2 round-trip |

Notes:
- **4a leans on fixtures, not round-trip.** The reader can only be verified in
  isolation against *known-good* `.mid` byte sequences (the writer doesn't exist
  yet). Hand-crafted minimal fixtures (one per event family, plus a multi-byte
  SysEx and a no-`MThd` negative) are the reader's evidence and directly catch
  #1/S2, #4/S4, S7, M2. `test/fixtures/` currently holds only `.gitkeep`.
- **4b is verified by writing then re-reading** with the now-trusted 4a reader
  (`read(write(read(F)))`), which catches the write-side Blockers #2/S3 (seq_name
  byte), #3 (track_end shape), #6/S5 (format word).
- **4c is the belt-and-suspenders**: the property over generated sequences is the
  test the CDC eval names as "the test that should exist regardless." It can only
  run once both halves land, hence the dependency on 4a and 4b.
- **Only slice 4a's full doc set (plan + cc-prompt + ledger) is authored now.**
  Slices 4b and 4c get their cc-prompt + ledger written when their predecessor
  closes, so later specs reflect what actually landed (disclosed deferral, not a
  silent drop — the *plan* for all three is here).

## Vocabulary adoption (the M1 fix, applied to `midifile`)

`midifile` drops every legacy shape and tag:

- **Containers:** `{seq, Header, ConductorTrack, Tracks}` → `#seq{format,
  division, tracks}`; `{track, Events}` → `#track{events}`; bare
  `{Name, DeltaTime, Values}` event tuples → `#event{delta, message}` wrapping a
  canonical `message()`. The format-1 conductor track is simply `hd(tracks)`
  (DESIGN §9 — a disclosed consumer-coordination change for `midi`).
- **`division`** becomes typed: `{ppqn, N} | {smpte, Fps, Tpf}` (DESIGN §9),
  replacing the raw 16-bit int the old code mishandled for SMPTE division.
- **Tags:** `on`/`off`→`#note_on{}`/`#note_off{}`, `seq_name`→`#meta_track_name{}`,
  `tempo`→`#meta_set_tempo{}`, `time_signature`→`#meta_time_signature{}`,
  `seq_num`→`#meta_sequence_number{}`, the mis-tagged sequencer-specific →
  `#meta_sequencer_specific{}`, `unknown_meta`→`#meta_unknown{}`, etc. (the full
  table is DESIGN §5).
- **Channel base:** read converts the 0..15 nibble to 1..16; write converts back.
  The `±1` lives only at the file edge (DESIGN §3.3, fixes #13/C7).
- **Normalization stays out of the codec:** a Note-On with velocity 0 reads as
  `#note_on{velocity = 0}`, never folded to a note-off and never inventing release
  velocity 64 (fixes #7/S8/C8; the fold is `midi`'s job).

## Error contract (the cluster-A failure-representation fix)

- `read/1 :: file:name_all() -> {ok, #seq{}} | {error, midierrs:reason()}` —
  replaces the untagged `{Path, Error}` (#15) and covers malformed *content*, not
  just open failure (M2): no `MThd` → `{error, {not_midi_file, Path}}`; a
  malformed VLQ → `{error, {bad_vlq, Bin}}` (S7); `file:open` failure →
  `{error, {open, Path, Reason}}`.
- `write/2 :: (#seq{}, file:name_all()) -> ok | {error, midierrs:reason()}` —
  an oversized delta-time (≥ 2²⁸) is a structured `{error, {bad_value, delta, V}}`
  or a crash on a genuine bug, never `exit("string" ++ Int)` (#5/#17).
- Genuinely malformed binary input may still crash (let-it-crash, R4) — but the
  *predictable* cases above are values.

## OTP floor / docs stance

Follow the Arc-3 maintainer decision (closing-report row 12): **support OTP
22–29, kept portable.** `midifile` and `midi_codec` use `%%` comments + `-spec`,
**not** `-doc`/`-moduledoc` (those render only on OTP-27+; `midibin` set this
precedent). 

**Disclosed inconsistency to reconcile in Arc 6:** `midierrs.erl` (added in Arc 2)
*does* carry `-moduledoc`/`-doc`. It still compiles on the floor (they parse as
inert attributes pre-27, and it uses plain double-quoted strings, not
triple-quoted), so it is not a build break — but it is inconsistent with the
stated "no `-doc`" floor. Left as an Arc-6 (docs/polish) item, recorded here so it
is not a silent drop. Do **not** fix it inside Arc 4.

## Definition of done (arc)

- Blockers #1, #2, #3 each fixed, each with a test that fails on the old code and
  passes on the new (DoD of the release plan).
- `read(write(Seq)) =:= Seq` holds as a PropEr property over generated `#seq{}`.
- `midifile` speaks the canonical vocabulary end-to-end; channel base 1-based;
  no normalization in the codec; no process-dictionary state.
- `midifile` has real tests; the coverage floor is raised from 0 and enforced by
  `make check` / CI.
- Every Arc-4 finding (below) is closed in a slice or explicitly deferred with
  rationale.

### Findings this arc owns (from RELEASE-PLAN.md row 4)
#1/S2, #2/S3, #3 (Blockers); #4/S4, #6/S5, #15 (High); #5/#17, #7/S8/C8, S7,
#13/C7, M1 (Medium); #18 (cluster B, latent High); #19 (testing); #22, #26
(Low); M2. (#20 — the `midibin` tests that baked in the SysEx/batch bugs — was
already closed in Arc 3; it is listed in the release-plan row only as part of the
"untested surface" cluster and needs no further Arc-4 action.)

## Deviations / disclosures (no silent drops)

- **Arc 3's independent CDC verification was consciously skipped** (maintainer
  decision at Arc-4 planning). Arc 3 is treated as closed on CC's
  `closing-report.md` + green `check`; the CAP independence leg
  (`arc03-midibin/cdc-verification.md`) was not run. Recorded so the deviation
  from ledger discipline is visible, not buried. Arc 4's own slices retain the
  CC-closes / CDC-verifies-independently protocol.
- **`midibin` is re-touched** by the `midi_codec` extraction (slice 4a) —
  behaviour-preserving, guarded by its Arc-3 suite. Disclosed scope, not silent
  expansion.
- **Slices 4b/4c cc-prompt + ledger are deferred** to their predecessors' close
  (see slice-breakdown note).
