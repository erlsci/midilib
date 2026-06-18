# Arc 4 · Slice A — reader · ledger

Grep/test-verifiable acceptance rows. **CC** closes each with evidence (the
command run + its result); **CDC** re-verifies independently on close. Five-
iteration cap. A row that can't be met is surfaced as a disclosed deferral, never
silently dropped.

| # | Acceptance criterion | Evidence to cite | CC | CDC |
|---|----------------------|------------------|----|-----|
| 1 | `midi_codec` exists as a pure, framing-free module: no `file:`/`io:` calls, no `put`/`get`, no delta-time/running-status logic | `grep -nE 'file:\|io:\|put\(\|get\(' src/midi_codec.erl` → none; module compiles | ☑ | ☐ |
| 2 | `midi_codec` owns the message↔bytes mapping both directions, 1-based channels, the C5 channel-mode policy, and no-vel-0-fold | unit tests for `decode_message/2`+`encode_message/1` round-trip; `mono_mode_on`, a 120–127-odd-value→`#control_change{}`, and `0x9n note 0`→`#note_on{velocity=0}` cases pass | ☑ | ☐ |
| 3 | `midibin` refactored onto `midi_codec`; its public API unchanged; **the Arc-3 `midibin` suite passes unchanged** | `git diff test/midibin_tests.erl test/prop_midibin.erl` → no changes; `rebar3 eunit` (midibin) + `rebar3 proper` green | ☑ | ☐ |
| 4 | `midifile:read/1 :: file:name_all() -> {ok,#seq{}} \| {error, midierrs:reason()}` — no untagged `{Path, Error}` | `-spec` present; `grep -n '{Path' src/midifile.erl` → none; open-failure test returns `{error,{open,_,_}}` (#15) | ☑ | ☐ |
| 5 | **#1/S2:** a multi-byte SysEx track reads without desync; payload accounts for the `F0` byte | `sysex_multibyte_read_test` (payload ≥ 256 B, trailing `F7` stripped into `#sysex.data`) passes; fails on the pre-rewrite reader | ☑ | ☐ |
| 6 | **#4/S4:** `?META_SEQUENCER_SPECIFIC` (0x7F) → `#meta_sequencer_specific{}`, distinct from `#meta_track_name{}` (0x03) | `sequencer_specific_read_test` + `track_name_read_test` assert distinct records | ☑ | ☐ |
| 7 | **S7:** an unterminated 4-byte VLQ (all high bits set) → `{error,{bad_vlq,_}}`, not a value | `bad_vlq_read_test` passes | ☑ | ☐ |
| 8 | **M2:** a file that opens but has no `MThd` → `{error,{not_midi_file,Path}}`, not a crash/scan | `not_midi_file_test` passes | ☑ | ☐ |
| 9 | **#7/S8/C8:** Note-On velocity 0 reads as `#note_on{velocity=0}` (no fold, no invented release vel) | `note_on_vel0_faithful_read_test` passes | ☑ | ☐ |
| 10 | **#13/C7:** channels in read output are 1-based (1..16) | `channel_base_read_test` asserts a known nibble decodes 1-based | ☑ | ☐ |
| 11 | **M1:** read output uses canonical records/tags only — no legacy `on`/`off`/`seq_name`/`tempo`/3-tuple shapes | `grep -nE '\{(on\|off\|seq_name\|tempo\|time_signature),' src/midifile.erl` → none; tests assert `#note_on{}`/`#meta_*{}` | ☑ | ☐ |
| 12 | **#18:** no process dictionary in `midifile`; running-status state is threaded as arguments, reset per track | `grep -nE 'put\(\|get\(' src/midifile.erl` → none | ☑ | ☐ |
| 13 | **#22:** no per-byte `pread` chunk scan; file is read into a binary and parsed in memory | `grep -n 'look_for_chunk\|pread' src/midifile.erl` → none (or a single block read); whole-file parse path present | ☑ | ☐ |
| 14 | **#26:** the `?DPRINT` no-op macro is gone | `grep -n 'DPRINT' src/midifile.erl` → none | ☑ | ☐ |
| 15 | Division decodes to the typed form: `{ppqn,N}` or `{smpte,Fps,Tpf}` | `ppqn_division_test` + `smpte_division_test` pass | ☑ | ☐ |
| 16 | Format 0 and format 1 both read into `#seq{format=…}` with `tracks` as a uniform list (no conductor split) | `format0_read_test` + `format1_read_test` pass | ☑ | ☐ |
| 17 | One fixture per event family reads to the expected record (channel-voice, channel-mode, system, each meta) | `midifile_tests` family cases pass | ☑ | ☐ |
| 18 | A running-status run (status byte elided on subsequent events) reads correctly | `running_status_read_test` passes | ☑ | ☐ |
| 19 | eunit suite green (`midi_codec` + `midibin` + `midifile`) | `rebar3 eunit` output | ☑ | ☐ |
| 20 | `rebar3 check` (compile, xref, dialyzer, eunit, coverage) green; dialyzer clean on the three modules | `rebar3 as test check` → exit 0; `rebar3 as test dialyzer` → 0 warnings | ☑ | ☐ |

## Notes for the closer
- Rows 5–9 are the highest-value independent checks (the Blocker + the read-side
  correctness fixes). Each fixture should be a **visible inline binary** so the
  triggering bytes can be read in review, and where practical assert the case
  *fails on the pre-rewrite reader* (the release-plan DoD: "a test that fails on
  the old code and passes on the new").
- Row 3 is the contract that the `midi_codec` extraction was behaviour-preserving:
  if the Arc-3 `midibin` suite needed *any* edit to pass, that is a finding — stop
  and surface it, don't quietly edit the tests.
- Round-trip (`read(write(Seq))=:=Seq`) is **not** a slice-A row — the writer
  doesn't exist yet. It is slice C's keystone. Slice A proves the reader against
  fixtures only.
- If no Erlang toolchain is available in the authoring env, rows 3, 19, 20 are the
  ones that can't be run there (as in Arc 3) — name the gap per row; first
  `rebar3` run is the close-out.
