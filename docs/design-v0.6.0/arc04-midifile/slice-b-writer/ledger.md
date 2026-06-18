# Arc 4 · Slice B — writer · ledger

Grep/test-verifiable acceptance rows. **CC** closes each with evidence; **CDC**
re-verifies independently. Five-iteration cap. A row that can't be met is a
disclosed deferral, never a silent drop.

| # | Acceptance criterion | Evidence to cite | CC | CDC |
|---|----------------------|------------------|----|-----|
| 1 | `write/2 :: (#seq{}, file:name_all()) -> ok \| {error, midierrs:reason()}`; the `not_implemented` stub is gone; assembly is a pure `to_binary/1` | `-spec` + `grep -n not_implemented src/midifile.erl` → none; `to_binary/1` present | ☐ | ☐ |
| 2 | **#6/S5:** header emits the real `Seq#seq.format` and `length(tracks)` — format 0 and 2 are preserved, not rewritten as 1 | `format0_write_test`, `format2_write_test` read the written header back → same format; `grep -nE '0, *1' src/midifile.erl` (the old hardcode) → none | ☐ | ☐ |
| 3 | Division encodes as the inverse of `decode_division/2`: `{ppqn,N}` (hi bit clear) / `{smpte,Fps,Tpf}` (hi byte `256-Fps`) | `ppqn_write_test`, `smpte_write_test` round-trip the division | ☐ | ☐ |
| 4 | **#2/S3:** `#meta_track_name{}` writes meta type `0x03`, never `0x2F`; a named track re-reads as `#meta_track_name{}` | `track_name_write_test` writes then reads back `#meta_track_name{}`; the bytes show `FF 03` | ☐ | ☐ |
| 5 | **#3:** `write(read(F)) `does not crash on End-of-Track; `#meta_end_of_track{}` round-trips (reader+writer share the record) | `eot_roundtrip_test`: `read(write(Seq)) =:= Seq` for a small fixed `Seq` ending in `#meta_end_of_track{}` | ☐ | ☐ |
| 6 | **#7:** a note-off with non-zero release velocity is written **with that velocity** (no forced 0); full status on every event | `note_off_velocity_preserved_write_test` (write `#note_off{velocity=64}`, read back velocity 64) | ☐ | ☐ |
| 7 | Explicit status byte on every channel event — no running-status elision; no process dictionary in the writer | `grep -nE 'put\(\|get\(' src/midifile.erl` → none; a two-note-on track writes two `0x9n` status bytes (byte assertion) | ☐ | ☐ |
| 8 | SysEx framed `F0 vlq(byte_size(D)+1) D F7`; a payload **ending in `F7`** still round-trips | `sysex_write_roundtrip_test` incl. `#sysex{data = <<…,16#F7>>}` → equal after `read(write(...))` | ☐ | ☐ |
| 9 | **#5/#17:** a delta-time ≥ 2²⁸ → `{error, {bad_value, delta, V}}`, never `exit/1`; `grep` shows no `exit(` in the writer | `oversized_delta_write_test`; `grep -n 'exit(' src/midifile.erl` → none | ☐ | ☐ |
| 10 | **N-CDC-1:** out-of-range record fields → `{error, {bad_value, Field, Value}}` uniformly (e.g. `#meta_sequence_number{value=70000}`, channel 0/17, data byte 200, non-power-of-two time-sig denom) — no `function_clause` crash on bad external data | `midi_codec_tests` bad-value cases (seq-num, channel, data byte, denom) all return `{error,{bad_value,_,_}}` | ☐ | ☐ |
| 11 | **#13/C7:** channel `-1` to the 0..15 nibble happens only in `midi_codec`; the writer never manipulates raw nibbles | grep: writer has no `band 16#0F` / `- 1` channel math; round-trip preserves channel 1..16 | ☐ | ☐ |
| 12 | **M1:** writer consumes canonical records only — no legacy event tuples | `grep -nE '\{(on\|off\|seq_name\|tempo),' src/midifile.erl` → none | ☐ | ☐ |
| 13 | An assembly error yields `{error,_}` with **no partial file** on disk (build in memory, then `file:write_file/2`) | `write_error_leaves_no_file_test` (force a `{bad_value,_}`; assert the target path does not exist) | ☐ | ☐ |
| 14 | A multi-event, multi-track `#seq{}` round-trips: `read(write(Seq)) =:= Seq` (fixed fixtures across event families) | `roundtrip_fixtures_test` (channel-voice, channel-mode, meta, sysex, multi-track) | ☐ | ☐ |
| 15 | The slice-A `midifile` reader suite **and** the Arc-3 `midibin` suite stay green (no regression from the `midi_codec` encode changes) | `git diff` on those test files → empty; `rebar3 eunit` all pass | ☐ | ☐ |
| 16 | `rebar3 check` green; `rebar3 proper` green; dialyzer clean on `midifile` + `midi_codec` | `rebar3 as test check` → exit 0; `dialyzer` → 0 warnings | ☐ | ☐ |

## Notes for the closer
- Rows 4, 5, 6 are the Blocker/High proofs — assert them as **write-then-read**
  through the now-trusted slice-A reader, and where practical show the failing
  bytes the old writer produced (`FF 2F` for a name; the `function_clause` on
  `{track_end,_,_}`) so the "fails on old code, passes on new" DoD is visible.
- Row 14 is fixed-fixture round-trip, **not** the PropEr property (that is slice C).
- Row 15 is the regression guard: the N-CDC-1 changes are in `midi_codec`'s encode
  side; `midibin` only uses `encode_message/1`, so confirm its behaviour is
  unchanged (only *added* `{bad_value,_}` on previously-crashing inputs).
- If no toolchain in the authoring env, rows 15/16 are the run-only ones — name the
  gap; first `rebar3` run closes them (as in slices A).
