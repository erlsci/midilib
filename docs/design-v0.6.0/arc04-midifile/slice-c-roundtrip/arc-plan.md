# Arc 4 · Slice C — round-trip property + coverage + error paths · plan

**Release:** v0.6.0 · **Arc:** 4 of 6 · **Slice:** C (round-trip) of 3 · **Depends on:** Slice A, Slice B
**Arc plan:** [`../arc-plan.md`](../arc-plan.md) ·
**Design of record:** [`../../arc02-canonical-vocabulary/DESIGN-vocabulary.md`](../../arc02-canonical-vocabulary/DESIGN-vocabulary.md)
**Status:** planning

## Goal

Lock the `midifile` rewrite shut and prove it stays shut: the keystone PropEr
property `read(write(Seq)) =:= Seq` over generated sequences, a real coverage floor
(replacing the `--min_coverage=0`, M4), and the completion of the reader's error
contract (the corruption-as-value decision carried from CDC). This is the slice the
CDC evaluation named: "the `read(write(Seq)) =:= Seq` round-trip … the test that
should exist regardless."

## What ships

### 1. `test/prop_midifile.erl` — the round-trip property

`prop_roundtrip`: `?FORALL(Seq, seq(), from_binary(to_binary(Seq)) =:= {ok, Seq})`,
run through the **pure** `to_binary/1` (slice B) and a new pure **`from_binary/1`**
(slice C — a filesystem-free read from an in-memory binary; `read/1` becomes
`file:read_file/1` + `from_binary/1`). `from_binary/1` is also a genuinely useful
public addition (read a MIDI image already in memory).

The generator is the design heavy-lift; it must produce only **round-trippable,
file-domain** values (subtleties that, if missed, make the property lie):

- **`seq()`** — `format ∈ 0..2`; format 0 ⇒ exactly one track; 1/2 ⇒ ≥ 1 track.
- **`division()`** — `{ppqn, range(1, 32767)}` | `{smpte, oneof([24,25,29,30]),
  range(1, 255)}`.
- **`track()`** — events ending in exactly one `#event{message =
  #meta_end_of_track{}}` (the documented invariant; the writer is faithful, so the
  generator must supply the EoT).
- **`event()`** — `delta ∈ range(0, 16#0FFFFFFF)` (the VLQ ceiling; oversized
  deltas are a *negative* test, not the property), `message = file_message()`.
- **`file_message()`** — **only** channel-voice, channel-mode, meta, and sysex.
  System-common / real-time are **excluded** (they are not SMF track content; the
  property is about files). Per-type constraints that make round-trip exact:
  - `#control_change{control ∈ 0..119}` — controllers 120–127 at canonical values
    decode to `#channel_mode{}` (the C5 policy), so a `control_change` there would
    *not* round-trip. (Same restriction Arc-3's `prop_midibin` used.)
  - `#channel_mode{}` — `value = 0` for every mode **except** `mono_mode_on`,
    which carries `range(0,127)` (the other modes force their value byte on encode,
    so a non-zero `value` would be lost).
  - `#meta_time_signature{denominator ∈ {1,2,4,8,16,32,64,128}}` (a power of two;
    others are `{error,{bad_value,…}}`), other fields in their 1..255 ranges.
  - text records (`#meta_text{}`/`copyright`/`track_name`/`instrument`/`lyric`/
    `marker`/`cue_point`) — **binary** payloads only (a `chardata()` *list* would
    round-trip to a binary and fail `=:=`).
  - `#meta_unknown{type ∈ 0..127 minus the modelled types, data = binary()}` — a
    modelled type would decode to its specific record, not `#meta_unknown{}`.
  - `#sysex{data = binary()}` — unrestricted (the `F0 vlq(size+1) D F7` framing
    round-trips any payload, including one ending in `F7`).
  - channel fields `1..16`, data bytes `0..127`, 14-bit fields `0..16383`,
    `#meta_sequence_number{0..65535}`, `#meta_smpte_offset{}` within field ranges,
    `#meta_channel_prefix{1..16}`, `#meta_key_signature{key −7..7, mode
    major|minor}`.

### 2. Coverage floor (closes M4)

- **Fix the measurement:** `rebar3 check`'s coverage currently reads
  `proper.coverdata` only (it never sees eunit — slice-A/B closing reports flagged
  this). Aggregate eunit + proper cover data so the reported number reflects the
  whole suite.
- **Raise the floor** from `--min_coverage=0` to a real threshold (recommend
  **≥ 85%**, maintainer sets the exact gate), enforced by `make check` / CI.
- The error-path work below is what makes the floor reachable — the previously
  uncovered `midifile` lines were the let-it-crash paths; turning the foreseeable
  ones into tested `{error,_}` values covers them.

### 3. Reader corruption-as-value — the carried contract decision

**Contract decision (recommended; confirm before dispatch — it evolves the
slice-A reader contract).** Promote foreseeable *file corruption* from a crash to a
structured `{error, midierrs:reason()}` value, so `read/1`/`from_binary/1` over an
openable file returns `{ok, #seq{}} | {error, reason()}` and crashes **only** on a
genuine internal invariant violation (a bug), never on bad external data. This is
the same principle that drove F-CDC-1 (alien chunks) and matches the project's
stated "let-it-crash is for the *unexpected*" stance — a truncated download or a
partial write is foreseeable I/O corruption, not a programmer error.

Promote these reader paths to values (add reasons to `midierrs:reason()` +
`format_error/1`; leave the module's `-doc` attributes alone — that's Arc 6):
- a chunk whose declared length overruns the buffer → `{error, {truncated, chunk}}`
  (or `{bad_chunk, …}`);
- the header declaring more `MTrk` chunks than the file holds → `{error,
  {missing_tracks, Got, Want}}`;
- a zero / invalid division → `{error, {bad_division, Word}}`;
- a truncated event / meta / sysex body → `{error, {truncated, Where}}`;
- a data byte with no running status to apply → `{error, {bad_running_status, _}}`.

**This changes the slice-A `truncated_chunk_still_crashes_test`** — it becomes an
`{error,_}` expectation and is renamed (e.g. `truncated_chunk_is_error_test`). That
is a *deliberate, disclosed* contract change, not a quiet test edit — call it out
in the closing report.

*(If the maintainer declines the promotion, slice C still ships the property +
coverage + negative tests asserting the current crash behaviour; say so and stop.)*

### 4. Negative-path + format round-trip tests

Negative tests for every promoted reason above (each an `{error,_}` assertion, no
longer `?assertError`), plus explicit `read(write(Seq)) =:= Seq` fixtures for
**format 0, 1, and 2** (the #6/S5 preservation, now exercised end-to-end).

## Findings closed (round-trip slice)

| Finding | How |
|---|---|
| **#19** (testing) | `prop_roundtrip` over generated `#seq{}` — the defect-dense module now has the property the audit said was missing; locks Blockers #1/#2/#3 |
| **#20** (testing) | completes the "untested surface" closure (the `midibin` half was Arc 3) via the file round-trip |
| **M4** | coverage gate fixed (eunit+proper aggregated) and raised from 0 to a real floor, enforced by `make check`/CI |
| **CDC corruption-as-value** | foreseeable file corruption returns `{error, reason()}`; `read/1` crashes only on internal bugs |
| **#6/S5** (end-to-end) | format 0/1/2 round-trip fixtures confirm the writer preserves format |

## Decisions made here

- **`from_binary/1` added** (pure, public) so the property is filesystem-free and
  `read/1` is `file:read_file/1` + `from_binary/1`; symmetric with `to_binary/1`.
- **Generator restricted to round-trippable file-domain values** (above) — a
  generator that emits non-round-trippable values would produce a property that
  fails for the wrong reasons; the restrictions are correctness, not convenience,
  and each is annotated with *why*.
- **Corruption → values** (recommended; maintainer-confirm) — see §3.

## Out of scope

`midimsg`/`midiutil` (Arc 5); the `midierrs` `-doc`/`-moduledoc` reconciliation
(Arc 6); running-status compression in the writer (deferred, slice B note).

## Arc closeout (after this slice)

Slice C closing the loop closes Arc 4. Its closing report should include the
**arc-level reconciliation**: every Arc-4 finding from `RELEASE-PLAN.md` row 4
(#1/S2, #2/S3, #3, #4/S4, #5/#17, #6/S5, #7/S8/C8, S7, #13/C7, #15, #18, #19, #22,
#26, M1, M2) shown closed in slice A, B, or C — the release plan's "no silent
drops" gate for the arc.

## Validation note

The property + coverage are inherently run-only; if the authoring env has no
toolchain, the generator and tests are authored against the spec and the gap named
per row, with the first `rebar3 proper`/`check` run as close-out (as in slices A/B).
The generator's round-trip restrictions are verifiable by reasoning even before
execution, but execution is the closing evidence.
