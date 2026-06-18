# Arc 4 · Slice A — `midi_codec` + `midibin` refactor + `midifile:read/1` · plan

**Release:** v0.6.0 · **Arc:** 4 of 6 · **Slice:** A (reader) of 3 · **Depends on:** Arc 3
**Arc plan:** [`../arc-plan.md`](../arc-plan.md) ·
**Design of record:** [`../../arc02-canonical-vocabulary/DESIGN-vocabulary.md`](../../arc02-canonical-vocabulary/DESIGN-vocabulary.md)
**Status:** planning

## Goal

Stand up the shared, framing-free message codec (`midi_codec`), refactor `midibin`
onto it without changing its behaviour, and build a correct, state-threaded,
canonical-vocabulary `midifile:read/1`. The reader is verified against hand-built
known-good byte fixtures (the writer doesn't exist yet, so round-trip verification
waits for slice C).

## What ships in this slice

### 1. `src/midi_codec.erl` — the pure message↔bytes core (new, internal)

Stateless, no I/O, no framing, no running-status, no delta-time. It owns the
canonical byte layout for channel-voice, channel-mode, system, and meta messages,
so the wire and file framers can never disagree on how a message is laid out (the
exact read/write drift the audit found).

Recommended API (a **starting point CC owns and may refine** — re-derive the
bit-syntax, don't trust this verbatim):

```erlang
%% Channel-voice / channel-mode / system messages.
%% Status is the full status byte (high nibble + channel for channel msgs).
-spec decode_message(Status :: 0..255, Data :: binary())
        -> {ok, message()} | {error, midierrs:reason()}.
-spec encode_message(message())
        -> {ok, {Status :: 0..255, Data :: binary()}} | {error, midierrs:reason()}.

%% How many data bytes a channel/system status byte consumes — the table the
%% file reader needs to apply running status. (0, 1, 2, or `variable` for F0/F7.)
-spec data_length(Status :: 0..255) -> 0 | 1 | 2 | variable.

%% Meta records <-> {meta type byte, payload bytes}. File-only, but homed here so
%% the vocabulary↔bytes mapping has one home. Framing (FF, VLQ length) is midifile's.
-spec decode_meta(Type :: 0..127, Payload :: binary())
        -> {ok, meta_message()} | {error, midierrs:reason()}.
-spec encode_meta(meta_message()) -> {ok, {Type :: 0..127, Payload :: binary()}}.
```

`midi_codec` carries the invariants that used to be scattered:
- **1-based channels** (the `±1` nibble math is here, applied once).
- **The C5 channel-mode policy** (controllers 120–127 at canonical values →
  `#channel_mode{}`, otherwise `#control_change{}`; `mono_mode_on` carries the
  count) — moved out of `midibin` so `midifile` inherits it identically.
- **No vel-0 fold** (`0x9n note 0` → `#note_on{velocity = 0}`).
- **Meta value conversions**: `meta_set_tempo` 24-bit µs/qn; `meta_time_signature`
  human denominator ↔ `2^dd` exponent; `meta_key_signature` `-7..7`/`major|minor`;
  `meta_unknown` round-trips any unmodelled type verbatim.

This slice implements **both directions** of `midi_codec` (encode is cheap to add
alongside decode and the writer slice will need it); the writer slice consumes the
encode side.

### 2. `src/midibin.erl` — refactor onto `midi_codec` (behaviour-preserving)

`midibin` keeps its exact public API (`decode/1`, `encode/1`, `encode_batch/1`,
`decode_batch/1`) and its Arc-3 contract, but delegates the body mapping to
`midi_codec`: it splits a complete wire message into `{Status, Data}`, calls
`midi_codec`, and keeps only the *wire framing* (single-byte real-time messages,
the `F0 … F7` SysEx framing, one-complete-message-per-call). **The Arc-3
`midibin` suite (`test/midibin_tests.erl`, `test/prop_midibin.erl`) must stay
green unchanged** — that is the proof the extraction preserved behaviour.

### 3. `src/midifile.erl` — `read/1` rewrite

```erlang
-spec read(file:name_all()) -> {ok, #seq{}} | {error, midierrs:reason()}.
```

Design:
- **Read the whole file into a binary, then parse functionally** (kills #22's
  byte-at-a-time `file:pread` scan and makes the parser pure and unit-testable).
  `file:open` failure → `{error, {open, Path, Reason}}` (#15).
- **Header:** match `<<"MThd", 6:32, Format:16, NumTracks:16, Division:16, …>>`.
  No `MThd` → `{error, {not_midi_file, Path}}` (M2). Decode `division`: high bit
  0 → `{ppqn, N}`; high bit 1 → `{smpte, Fps, Tpf}` (DESIGN §9).
- **Tracks:** match `<<"MTrk", Len:32, TrackData:Len/binary, …>>` × `NumTracks`.
- **Events (state-threaded, NO process dictionary):** thread `{Status, Chan}` as
  an accumulator argument through the per-track event loop, **reset at each track
  start** (#18). For each event: read the VLQ delta-time, recover the status byte
  (applying running status for data-first bytes), use `midi_codec:data_length/1`
  to take the right number of data bytes, and call `midi_codec:decode_message/2`.
  Running status is **cleared after meta/sysex/system-common** (spec-correct; the
  old code left it at `0xFF`/`0xF0` — audit clean-check #1 residual note).
- **Meta:** `FF Type <vlq-len> <payload>` → `midi_codec:decode_meta/2`, wrapped
  `#event{delta, message}`.
- **SysEx (#1/S2 fix):** `F0 <vlq-len> <payload-incl-trailing-F7>`. Account for
  the `F0` status byte: payload starts at `pos + 1 + vlq_len_bytes`, consumption
  is `1 + vlq_len_bytes + Length`. Strip the trailing `F7` into `#sysex{data}`.
- **VLQ reader (S7 fix):** 1–4 bytes; an all-high-bits (unterminated) 4-byte
  sequence is `{error, {bad_vlq, Bin}}`, not a silently-accepted value.
- **Result:** `#seq{format, division, tracks = [#track{events = [#event{}]}]}`.
  No conductor-track split — format-1's conductor is `hd(tracks)` (DESIGN §9).

### 4. `test/fixtures/` + `test/midifile_tests.erl`

Hand-built known-good byte sequences (inline binaries in the suite, so the exact
triggering bytes are visible in review) covering: one fixture per event family;
a **multi-byte SysEx** track that the old reader desyncs on (#1/S2); a
**sequencer-specific** meta distinct from a track-name (#4/S4); an **unterminated
VLQ** negative (S7); a **no-`MThd`** negative (M2); a **running-status** run;
**SMPTE division**; and a **format-0** file. Each asserts `read/1` returns the
expected `#seq{}` (or the expected `{error, _}`).

## Findings closed (reader slice)

| Finding | How |
|---|---|
| **#1 / S2** (Blocker) | SysEx read accounts for the `F0` byte; multi-byte SysEx fixture reads without desync |
| **#4 / S4** (High) | `?META_SEQUENCER_SPECIFIC` → `#meta_sequencer_specific{}`, distinct from `#meta_track_name{}` |
| **#15** (High, read side) | `read/1` → `{ok, #seq{}}` / `{error, {open, Path, R}}` — no untagged `{Path, Error}` |
| **M2** | missing `MThd` → `{error, {not_midi_file, Path}}`, not a byte-scan crash |
| **S7** (Medium) | unterminated 4-byte VLQ → `{error, {bad_vlq, Bin}}` |
| **#7 / S8 / C8** (read side) | Note-On vel 0 reads faithfully as `#note_on{velocity=0}`; no invented release vel 64 |
| **#13 / C7** (read side) | channels are 1-based (via `midi_codec`); `±1` only at the file edge |
| **M1** (read side) | canonical records/tags throughout; legacy `{Name, DT, Vals}` gone |
| **#18** (read side) | running-status state threaded as an argument; no `put`/`get` |
| **#22** (Low) | whole-file read + in-memory parse; no per-byte `pread` scan |
| **#26** (Low) | rewrite drops the `?DPRINT` no-op macro entirely |
| **#19** (read side) | first real `midifile` test suite + fixtures |

## Decisions made here

- **`midi_codec` is internal**, not added to any public API doc; `midibin` and
  `midifile` are the public surfaces. It is not in `midilib.app.src`'s public
  contract beyond being a module in the app.
- **Both `midi_codec` directions ship in slice A** even though the reader only
  needs decode — the writer slice would otherwise re-open the module, and shipping
  the pair lets slice A's `midibin` refactor exercise encode too (its suite covers
  both directions).
- **Whole-file read** over streaming `pread`. MIDI files are small; a pure parser
  over an in-memory binary is simpler, faster (#22), and unit-testable without a
  filesystem (the fixtures are binaries, tested via an internal parse entry point
  if `read/1`'s file I/O is factored from the pure parser).
- **Running status cleared after meta/sysex/system-common** — spec-correct and
  removes the latent malformed-nibble path the audit flagged.

## Out of scope (this slice)

`midifile:write/2` (slice B); the round-trip PropEr property and coverage floor
(slice C); any `midimsg`/`midiutil` change (Arc 5); the `midierrs` `-doc`
inconsistency (Arc 6); the OTP-floor *decision* (settled in Arc 3 — keep portable).

## Validation note (honesty / capability)

If the authoring environment has a toolchain, every ledger row is run and cited.
If not, the parser and suite are authored against the spec + the Arc-3 idioms and
the gap is named per row (as Arc 3 did) — the natural close-out is the first
`rebar3 check` run. The reader's correctness on the Blocker (#1/S2) is provable
from the fixture bytes by inspection even before execution, but execution is the
closing evidence.
