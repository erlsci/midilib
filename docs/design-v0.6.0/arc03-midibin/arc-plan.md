# Arc 3 — `midibin` onto the canonical vocabulary · arc plan

**Release:** v0.6.0 · **Arc:** 3 of 6 · **Depends on:** Arc 2 · **Status:** in progress
**Design of record:** [`../arc02-canonical-vocabulary/DESIGN-vocabulary.md`](../arc02-canonical-vocabulary/DESIGN-vocabulary.md)

## Goal

Move the wire codec onto bare canonical records and the `{ok,_}|{error,_}`
contract, and land the SysEx fix. `midibin` is the small, clean, already-tested
module, so it is the cheapest place to prove the new vocabulary before the
larger `midifile` rewrite (Arc 4) leans on it.

## New public API (breaking)

```erlang
decode/1       :: binary()      -> {ok, message()} | {error, {unknown, binary()}} | {error, non_midi}
encode/1       :: message()     -> {ok, binary()}  | {error, midierrs:reason()}
encode_batch/1 :: [message()]   -> [binary() | {error, midierrs:reason()}]   %% short-circuits, trailing error (R9)
decode_batch/1 :: [binary()]    -> [{ok, message()} | {error, _}]
```

No `{midi, _}` envelope; bare records in and out. Channels are 1-based at this
boundary; the `±1` nibble conversion stays internal to `midibin`.

## Findings closed

- **#8 / S1 / C1 (Blocker)** — SysEx is now arbitrary-length: decode
  `<<16#F0, Payload/binary, 16#F7>>` → `#sysex{data=Payload}`; encode
  `#sysex{data=D}` → `<<16#F0, D/binary, 16#F7>>`. The single-7-bit-byte clause
  is gone; encode of a binary no longer throws.
- **#11 / C3 (High)** — batch is a list operation (`encode_batch/1` /
  `decode_batch/1`); the `{midi,{batch,[{id,_},{messages,_}]}}` shape is retired.
  `midimsg:batch` is realigned to produce `[message()]` in Arc 5.
- **#14 / C4** — one failure convention: decode → `{error,{unknown,Bin}}`,
  encode → `{error, reason()}`. No more `{unknown,Bin}` vs `{error,_}` fork.
- **#16** — error reasons are atoms/terms (`midierrs:reason()`); strings live in
  `midierrs:format_error/1`. `include/errors.hrl` is removed.
- **#21** — `encode_batch`/`decode_batch` use a reversed accumulator / list
  comprehension; no `Acc ++ [X]`.
- **#20** — tests no longer bake in the single-byte SysEx or the dead batch shape.
- **C5 (decode completeness) — documented policy:** controllers 120–127 with
  their canonical channel-mode values decode to `#channel_mode{}`; with any other
  value they decode to `#control_change{}` (faithful to the bytes — they *are* a
  CC on the wire). `mono_mode_on` (126) is now **implemented** (was
  `not_implemented`), carrying the channel-count byte in `#channel_mode.value`.
  Anything unrecognised → `{error,{unknown,Bin}}`. This is the "decide and
  document" C5 asked for.

## Decisions made here

- **Range guards, total encode.** Encode clauses guard channel (1..16) and data
  bytes (0..127 / 0..16383); a record whose fields are out of range, or a
  non-wire message (any `#meta_*{}`), falls to `encode(T) -> {error,{unsupported,T}}`
  (and non-records → `{error, non_midi}`). Prevents silent bit-syntax truncation
  without crashing the caller. (Per-field `{bad_value,_,_}` precision is reserved
  in the vocabulary for a later pass.)
- **Meta records are not wire-encodable** — `encode/1` returns
  `{error,{unsupported, Rec}}` for any `#meta_*{}`; they only serialise via
  `midifile` (Arc 4).
- **Clause ordering preserved (FP-12):** specific channel-mode clauses precede
  the generic CC clause; single-byte system/real-time clauses are unambiguous by
  first byte. (The audit's clean-check #5 confirmed the old ordering was sound;
  the rewrite keeps that discipline.)

## Acceptance criteria (ledger steps)

1. Every message type `decode/1` produces, `encode/1` round-trips, and vice
   versa (decode/encode cover the same set) — asserted by eunit + the PropEr
   property.
2. A multi-byte SysEx (e.g. a 300-byte payload) round-trips:
   `decode(encode(#sysex{data=D})) =:= {ok, #sysex{data=D}}`.
3. `encode_batch/1` on a list with one bad element returns the good binaries
   followed by a single trailing `{error,_}` and stops (R9 behaviour).
4. `decode/1` of unrecognised bytes returns `{error,{unknown,Bin}}`; no clause
   silently reinterprets a 120–127 controller carrying an odd value as a mode.
5. No reference to `?ERR_*` or `include/errors.hrl` remains; `errors.hrl` deleted.
6. `make check` passes on a machine with `rebar3` (see Validation note); the new
   `midibin` round-trip suite + PropEr property pass.

## Validation note (honesty / capability)

No Erlang toolchain in the authoring environment, so `midibin.erl`, the eunit
suite, and the PropEr module are authored against the spec + the existing idioms
but are **not compiled or run here**. Criteria 1–4 and 6 are verified on first
`rebar3` run (the natural close-out check for this arc). Static
parity/consistency checks (decode↔encode message-set match, record refs, bracket
balance) are run in-place and reported. Gap named, not hidden.
