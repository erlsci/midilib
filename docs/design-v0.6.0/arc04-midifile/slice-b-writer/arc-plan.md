# Arc 4 · Slice B — `midifile:write/2` · plan

**Release:** v0.6.0 · **Arc:** 4 of 6 · **Slice:** B (writer) of 3 · **Depends on:** Slice A
**Arc plan:** [`../arc-plan.md`](../arc-plan.md) ·
**Design of record:** [`../../arc02-canonical-vocabulary/DESIGN-vocabulary.md`](../../arc02-canonical-vocabulary/DESIGN-vocabulary.md)
**Status:** planning

## Goal

Rebuild `midifile:write/2` against the canonical vocabulary, on top of
`midi_codec` (the shared message↔bytes core slice A introduced). The writer is the
inverse of the slice-A reader; sharing the byte mapping is what guarantees the
read/write paths can never disagree again — the root of Blockers #2 and #3.

`write/2` is currently a slice-B stub (`{error, not_implemented}`).

## What ships

### `src/midifile.erl` — `write/2`

```erlang
-spec write(#seq{}, file:name_all()) -> ok | {error, midierrs:reason()}.
```

Structure (mirror the reader's shape — a pure builder + thin I/O):

- **`write/2` = build a binary, then `file:write_file/2`.** Factor the assembly
  into a **pure `to_binary/1` (#seq{} -> {ok, binary()} | {error, reason()})** so
  the binary can be produced and checked without touching the filesystem. Slice C's
  round-trip property uses this pure path (as the reader's pure `parse/2` is used
  by slice-A fixtures).
- **Header (#6/S5 fix):** emit `MThd 6:32 Format:16 NumTracks:16 Division`, with
  `Format = Seq#seq.format` (the real format, **not** a hardcoded `0,1`) and
  `NumTracks = length(Seq#seq.tracks)`. Division is the inverse of the reader's
  `decode_division/2`: `{ppqn, N}` -> 16-bit, high bit clear; `{smpte, Fps, Tpf}`
  -> high byte `(256 - Fps)` (high bit set, two's-complement negative frame rate),
  low byte `Tpf`.
- **Tracks:** each `#track{events}` -> `MTrk Len:32 <event bytes>`, where `Len` is
  the byte size of the assembled event bytes (compute from the built iolist/binary,
  not by guessing). 
- **Events:** for each `#event{delta, message}`, emit `vlq(Delta)` then the
  message bytes. Message bytes come from `midi_codec`:
  - channel / system messages: `midi_codec:encode_message/1` -> `{Status, Data}`,
    emit `<<Status, Data/binary>>`.
  - meta: `midi_codec:encode_meta/1` -> `{Type, Payload}`, emit
    `<<?STATUS_META_EVENT, Type, (vlq(byte_size(Payload)))/binary, Payload/binary>>`.
  - SysEx: `#sysex{data = D}` -> `<<?STATUS_SYSEX, (vlq(byte_size(D) + 1))/binary,
    D/binary, ?STATUS_EOX>>`. **The length includes the trailing `F7`** so the
    reader's `parse_sysex`/`strip_eox` round-trips *any* payload (even one whose
    own last byte is `F7`). This is the write-side counterpart of the slice-A
    SysEx accounting.
- **VLQ writer:** 1–4 bytes for values `0 .. 16#0FFFFFFF`. A delta-time outside
  that range is **not** `exit("string" ++ Int)` (#5/#17 — that clause is deleted):
  it is a structured `{error, {bad_value, delta, V}}` bubbled up through `write/2`
  (see Error stance).

### Running status: explicit status bytes (no compression)

The writer emits an **explicit status byte for every channel event** — it does
**not** use running-status elision. Rationale:

- Round-trip is **semantic** (`read(write(Seq)) =:= Seq`), not byte-identity: the
  slice-A reader expands running status into complete messages, so a full-status
  file reads back to the same `#seq{}` a running-status file would. Compression
  buys smaller files, nothing the consumer (`midi`) needs.
- It **structurally eliminates #7** (the old writer forced `OutVel = 0` inside the
  running-status branch, destroying note-off release velocity). With full status +
  full data on every event, there is no elision branch to get wrong.
- It keeps the writer free of the threaded status/channel state that was the
  writer half of cluster B (#18).

(If a future arc wants running-status compression for size, the hard invariants
are: thread the status as an **argument** — never the process dictionary — and
elide only the *status byte*, never data bytes. Deferred, not built here.)

### Error stance — resolves N-CDC-1

One uniform encode-side convention, applied in `midi_codec` and bubbled through
`write/2`:

- An **out-of-range field** on any record handed to the writer
  (`#meta_sequence_number{value > 65535}`, an out-of-range SMPTE field, a channel
  outside 1..16, a data byte > 127, a non-power-of-two time-signature denominator,
  a delta-time ≥ 2²⁸, …) -> `{error, {bad_value, Field, Value}}`. **No silent
  truncation, no `function_clause` crash on bad external data.**
  - `midi_codec:encode_meta/1` is extended so every modelled record validates its
    fields and returns `{bad_value, Field, _}` on range violation (today only the
    time-signature denominator does; the others `function_clause`-crash — that is
    N-CDC-1). `encode_message/1` already guards + falls to `{unsupported,_}` /
    `non_midi`; align it to emit `{bad_value, Field, _}` where a field is merely
    out of range (vs the term not being a message at all).
  - `write/2` short-circuits on the first `{error, _}` from the codec or the VLQ
    writer and returns it (no partial file written — build fully in memory first,
    then `file:write_file/2`).
- A genuine internal invariant violation (a bug) may still crash. The line:
  **bad external data is a value; a broken invariant is a crash.**

This touches `src/midi_codec.erl` (encode side) and its tests
(`test/midi_codec_tests.erl`) — additive (new `{bad_value,_}` cases). `midibin`
uses only `encode_message/1` for wire messages and is unaffected in behaviour
(its suite must stay green — re-confirm).

## Findings closed (writer slice)

| Finding | How |
|---|---|
| **#2 / S3** (Blocker) | `#meta_track_name{}` -> `encode_meta` -> type `0x03` (not `0x2F`); single source of truth in `midi_codec` |
| **#3** (Blocker) | reader and writer share the canonical `#meta_end_of_track{}` record; the 3-tuple/2-tuple shape mismatch is gone by construction. `write(read(F))` no longer `function_clause`-crashes |
| **#6 / S5** (High) | header emits the real `Seq#seq.format` and `length(tracks)`; format 0/2 are preserved |
| **#7** (Medium, write side) | full-status write preserves note-off velocity; no `OutVel = 0` branch |
| **#5 / #17** (Medium) | oversized VLQ -> `{error, {bad_value, delta, V}}`, never `exit/1` on a string |
| **#13 / C7** (write side) | channel `-1` to the 0..15 nibble happens only in `midi_codec`; writer never sees raw nibbles |
| **M1** (write side) | writer consumes canonical records only; no legacy event tuples |
| **#18** (write side) | no process dictionary; the writer is stateless over events (no running-status state) |
| **N-CDC-1** | uniform `{error, {bad_value, Field, Value}}` encode stance across `encode_message`/`encode_meta`/`write/2` |

## Decisions made here

- **Faithful writer:** writes exactly the events given — no auto-appended or
  auto-stripped End-of-Track. A well-formed `#seq{}` has each track ending in
  `#meta_end_of_track{}` (the reader guarantees this; document the invariant). This
  keeps `read(write(Seq)) =:= Seq` exact. (A CONSIDER for a later arc: a
  `midifile:ensure_eot/1` helper for hand-built sequences — not built here.)
- **Pure `to_binary/1`** so slice C's property is filesystem-free and `write/2`
  stays a thin `to_binary/1` + `file:write_file/2`.
- **Build fully in memory, then write** — so an error mid-assembly yields
  `{error, _}` with **no partial file** on disk.

## Out of scope (this slice)

The round-trip PropEr property, the coverage floor, and the **reader
corruption-as-value** contract change (truncated chunk / over-declared `ntrks` /
zero division -> `{error, _}`) are **slice C** — that change pairs with C's
coverage goal (those crash lines are exactly C's uncovered coverage) and its
negative-path tests. Flagged here so the carried CDC question is visibly routed,
not dropped. Also out: `midimsg`/`midiutil` (Arc 5); the `midierrs` `-doc`
reconciliation (Arc 6).

## Acceptance (ledger)

See [`ledger.md`](ledger.md). Headline rows: a hand-built `#seq{}` writes and
**re-reads through the slice-A reader to an equal `#seq{}`** (`read(write(Seq))`
for fixed fixtures — the cheap proof of #2/#3/#6 before slice C's property);
format 0/1/2 headers correct; note-off velocity preserved; oversized delta and
out-of-range fields return `{bad_value, _}`; `rebar3 check` green; the slice-A
reader **and** `midibin` suites stay green.

## Validation note

As in slice A: if a toolchain is present every row is run; if not, the writer +
suite are authored against the reader's byte fixtures (the reader is now trusted,
so write-then-read is a strong check) and the execution gap is named per row.
