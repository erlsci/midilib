# Arc 4 · Slice A — reader · CC closing report

**Slice:** A (reader) of 3 · **Author:** CC · **Status:** closed, ready for CDC verification
**Base commit:** `06a6f65` (branch `release/0.6.x`) · **Toolchain present:** Erlang/OTP 28
(emulator 16.x), rebar3 3.x — every ledger row was *run*, not authored-against-spec.

## What shipped

| File | Lines | Role |
|------|------:|------|
| `src/midi_codec.erl` | 364 | **new** — pure, framing-free message↔bytes core (both directions) |
| `src/midibin.erl` | 91 | refactored onto `midi_codec` (was 221) — wire framer only |
| `src/midifile.erl` | 251 | `read/1` rewrite (state-threaded, canonical); `write/2` stubbed for slice B |
| `test/midi_codec_tests.erl` | 182 | **new** — codec unit tests (row 2) |
| `test/midifile_tests.erl` | 275 | **new** — reader fixtures (rows 4–18) |
| `src/midilib.app.src` | +1 | added `midi_codec` to the module list (disclosed metadata change) |

The three-layer architecture from the arc plan is now real: `midi_codec` owns the
message↔bytes mapping; `midibin` (wire) and `midifile` (file) frame on top of it.
The read/write drift the audit found cannot recur because the byte layout has one
home.

## Headline results

- `rebar3 as test check` → **exit 0** (compile, xref, dialyzer, eunit, coverage).
- `rebar3 as test dialyzer` → **0 warnings** on all three modules.
- **107 eunit tests pass**; **2 PropEr properties pass** (100 cases each).
- The **Arc-3 `midibin` suite passes unchanged** — `git diff` on the two test
  files is empty (the proof the `midi_codec` extraction was behaviour-preserving).
- Coverage (eunit + proper combined): `midi_codec` 97%, `midibin` 93%,
  `midifile` 81%. (See the coverage note at the end — the `check` alias reports
  the *proper-only* number; it enforces no floor by design, per `rebar.config`.)

## Per-row evidence

| # | Criterion | Evidence | Verdict |
|---|-----------|----------|---------|
| 1 | `midi_codec` pure/framing-free | `grep -nE 'file:\|io:\|put\(\|get\('` → hits only in the module doc-comment, **none in code**; module compiles | ✅ |
| 2 | `midi_codec` owns both directions, ±1, C5 policy, no vel-0 fold | `midi_codec_tests`: `channel_voice_roundtrip`, `channel_mode_roundtrip`, `mono_mode_on_carries_count`, `controller_120_127_odd_value_is_cc`, `note_on_velocity_zero_not_folded`, `channel_base_1_based`, `meta_roundtrip` — all pass | ✅ |
| 3 | `midibin` refactored; API unchanged; Arc-3 suite green unchanged | `git diff test/midibin_tests.erl test/prop_midibin.erl` → **empty**; `rebar3 eunit`(midibin) 28/28, `rebar3 proper` 2/2 | ✅ |
| 4 | `read/1` typed return; no untagged `{Path, Error}` | `-spec read/1` present; `grep -n '{Path' src/midifile.erl` → none; `read_open_failure_test` → `{error,{open,Path,enoent}}` (#15) | ✅ |
| 5 | **#1/S2** multi-byte SysEx, no desync, F0 accounted | `sysex_multibyte_read_test`: 300-byte payload (2-byte VLQ len), trailing F7 stripped into `#sysex.data`, **and the following note-on reads correctly** (the desync proof) | ✅ |
| 6 | **#4/S4** sequencer-specific ≠ track-name | `track_name_read_test`, `sequencer_specific_read_test`, `sequencer_specific_distinct_from_track_name_test` assert distinct records | ✅ |
| 7 | **S7** unterminated 4-byte VLQ → `{error,{bad_vlq,_}}` | `bad_vlq_read_test` (four `0xFF` delta bytes) → `{error,{bad_vlq,_}}` | ✅ |
| 8 | **M2** no MThd → `{error,{not_midi_file,Path}}` | `not_midi_file_test` (a `"RIFF…"` blob) → `{error,{not_midi_file,_}}` | ✅ |
| 9 | **#7/S8/C8** Note-On vel 0 faithful | `note_on_vel0_faithful_read_test` → `#note_on{velocity=0}` (no fold, no vel 64) | ✅ |
| 10 | **#13/C7** channels 1-based | `channel_base_read_test`: nibble 0 → ch 1, nibble 15 → ch 16 | ✅ |
| 11 | **M1** canonical records only | `grep -nE '\{(on\|off\|seq_name\|tempo\|time_signature),' src/midifile.erl` → none; `canonical_records_only_test` asserts `#meta_track_name{}/#program_change{}/#note_on{}/#meta_end_of_track{}` | ✅ |
| 12 | **#18** no process dict; running status threaded, reset per track | `grep -nE 'put\(\|get\(' src/midifile.erl` → none; `running_status_read_test`, `running_status_reset_per_track_test` | ✅ |
| 13 | **#22** no per-byte `pread` scan | `grep -nE 'look_for_chunk\|pread' src/midifile.erl` → hit only in doc-comment, **none in code**; `read/1` is `file:read_file/1` + in-memory parse | ✅ |
| 14 | **#26** `?DPRINT` gone | `grep -n 'DPRINT' src/midifile.erl` → none | ✅ |
| 15 | Division typed `{ppqn,N}`/`{smpte,Fps,Tpf}` | `ppqn_division_test` → `{ppqn,480}`; `smpte_division_test` (`0xE7 0x28`) → `{smpte,25,40}` | ✅ |
| 16 | Format 0 & 1, uniform track list, no conductor split | `format0_read_test`, `format1_read_test` (2 tracks, both `#track{}`, no split) | ✅ |
| 17 | One fixture per event family | `channel_voice_families_test` (7), `channel_mode_family_test`, `meta_families_test` (14 incl. seq-num/text/copyright/instrument/lyric/marker/cue/chan-prefix/tempo/smpte/time-sig/key-sig×2/unknown), `end_of_track_family_test` | ✅ |
| 18 | Running-status run reads correctly | `running_status_read_test` (status elided on events 2 and 3) | ✅ |
| 19 | eunit green (codec + midibin + midifile) | `rebar3 eunit` → **All 107 tests passed** | ✅ |
| 20 | `rebar3 check` green; dialyzer clean | `rebar3 as test check` → **exit 0**; `rebar3 as test dialyzer` → **0 warnings** | ✅ |

## Design decisions I made and own

1. **`midi_codec` covers channel + channel-mode + system-common + system-real-time**
   as `{Status, Data}` pairs; only SysEx (F0…F7) framing stays in `midibin`. This
   is the cleanest single-source-of-truth split and lets `midibin` delegate every
   non-SysEx body. `midibin` reconstructs `{unknown, <<Status, Data>>}` on the
   delegated error path so its exact `{error,{unknown,Bin}}` contract (and its
   suite) is preserved byte-for-byte.
2. **`decode_meta/2` is total and `{ok,_}`-only.** Any unmodelled meta type — or a
   modelled type with an unexpected payload length — falls through to
   `#meta_unknown{}` (verbatim, lossless). I narrowed its spec to `{ok, meta_message()}`
   accordingly; the dead `{error,_}` branch this exposed in `midifile` was removed
   (dialyzer caught it). A meta type byte ≥ 128 (malformed; the SMF type byte is
   7-bit) has no clause and crashes — let-it-crash (R4).
3. **F7-led events** (SysEx "escape"/continuation) share the F0 VLQ framing and
   decode to `#sysex{}` (raw bytes, `strip_eox` only drops a *trailing* F7). The
   vocabulary has one SysEx record; faithful F7-escape *round-trip* is a writer
   (slice B) concern. Documented and tested (`sysex_f7_escape_read_test`).
4. **Malformed binary crashes, per the contract.** The predictable failures are
   values (`{open,_,_}`, `{not_midi_file,_}`, `{bad_vlq,_}`); genuinely malformed
   track bytes — a truncated event/meta/sysex, a data byte with no running status,
   a zero or non-MTrk chunk where a track is required, a zero division — crash with
   a descriptive `error(...)` term (R4). This keeps `read/1` honest: it does not
   manufacture a plausible-but-wrong `#seq{}` from garbage.
5. **`write/2` stubbed to `{error, not_implemented}`** (a `midierrs:reason()`), as
   the cc-prompt directed — the 2010 process-dict writer was deleted rather than
   carried as dead legacy code (it would not compile against `#seq{}` and would
   reintroduce the `put`/`get` and legacy tuples this arc removes).

## Disclosed changes / deferrals (no silent drops)

- **`src/midilib.app.src`** gained `midi_codec` in its `{modules, …}` list — a
  required metadata change for a new module, not a scope expansion. `midi_codec`
  remains internal (not a public API surface).
- **`midierrs.erl` and `midi_msg.hrl` were not touched** (as instructed). The
  `meta_unknown.type :: 0..127` and `division` `pos_integer()` constraints in the
  header drove two let-it-crash guards (meta type ≥ 128; zero division) so the
  reader never constructs an out-of-contract record — these satisfy both the type
  and dialyzer.
- **Coverage measurement gap (not a code gap).** The `coverage` alias computes its
  number from `proper.coverdata` only, so `rebar3 check` prints a low total (it
  has never seen the eunit run). The real reader coverage — eunit + proper
  combined — is `midi_codec` 97% / `midibin` 93% / `midifile` 81%, obtained via
  `rebar3 as test eunit --cover` then `rebar3 as test cover -v`. The uncovered
  `midifile` lines are the let-it-crash defensive paths in decision 4. The
  enforced floor is owned by the Makefile/CI (per `rebar.config`), which rises per
  arc; raising it is out of scope for slice A (slice C owns the coverage floor).
- **No round-trip property** in slice A — the writer does not exist yet; that is
  slice C's keystone, as the plan states.

## For CDC

The highest-value independent checks are rows 5–10 (the Blocker + read-side
correctness). Every fixture is an inline binary in `test/midifile_tests.erl`, so
the triggering bytes are reviewable without running anything. Row 3 is the
behaviour-preservation contract: `git diff test/midibin_tests.erl
test/prop_midibin.erl` must stay empty. Re-run `rebar3 as test check` and the
combined-coverage commands above to reproduce.
