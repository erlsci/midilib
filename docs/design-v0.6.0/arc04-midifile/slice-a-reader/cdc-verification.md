# Arc 4 · Slice A — reader · CDC verification

**Role:** CDC — independent leg of the CAP loop; **not** the doer (CC authored the
code and the closing report).
**Date:** 2026.06.17
**Base verified:** `release/0.6.x` @ `f834f11` (the three slice-A commits
`ee141bc` → `e69d8d3` → `f834f11`, on top of pre-slice-A `06a6f65`).
**Artifacts read in full:** `src/midi_codec.erl`, `src/midibin.erl`,
`src/midifile.erl`, `test/midi_codec_tests.erl`, `test/midifile_tests.erl`,
`include/midi_msg.hrl`, `include/midi.hrl`, `src/midierrs.erl`, the slice-A
`arc-plan.md` / `cc-prompt.md` / `ledger.md` / `closing-report.md`, and the audit
+ CDC-eval findings this slice closes.

**Capability disclosure (honesty / let-it-crash).** No Erlang toolchain is
installable in the CDC sandbox (no root: `sudo` blocked by `no-new-privileges`; no
`erl`/`rebar3`/`escript`; `apt` needs root). I therefore could **not** re-run
`rebar3 check`. Dispositions below are resolved one of two ways, stated per row:
**(grep/git)** — reproduced directly in the sandbox against the committed tree;
**(read)** — established by reading the code against Erlang semantics + the byte
arithmetic worked by hand. CC's OTP-28 green run stands as the *execution*
evidence for the three runtime-only rows (3, 19, 20); I did not independently
reproduce it, and say so rather than imply I did.

---

## 1. Verdict

**Slice A is fit to stand as closed — PASS / GO.** The three-layer split is real
and clean, every Blocker/High/Medium read-side finding this slice owns is genuinely
fixed (not papered over), and the evidence is reproducible from the committed
artifacts. The `midi_codec` extraction is behaviour-preserving: the Arc-3 `midibin`
test files are **byte-identical** from `06a6f65` to `f834f11` (independently
confirmed by `git diff`), and the refactored `midibin` routes every non-SysEx body
through the core while reconstructing its exact `{error,{unknown,Bin}}` contract.

One **Low** spec-robustness finding is added below (alien-chunk handling), plus two
**process/housekeeping** notes (the work is already committed, contrary to CC's
chat message; a stale `.git/index.lock`). None blocks closure.

## 2. Blocker re-derivation (the load-bearing checks)

- **#1/S2 — SysEx read off-by-one — FIXED (read).** The new reader matches
  `parse_event(<<?STATUS_SYSEX, Rest/binary>>, _)`, which *consumes* the `F0` byte
  in the pattern; `parse_sysex/1` then reads the VLQ length and takes exactly `Len`
  payload bytes. I traced the spec's own example `F0 05 43 12 00 07 F7`:
  match consumes `F0`; `read_vlq` → `Len=5`; `<<Payload:5/binary, _>>` →
  `43 12 00 07 F7`; `strip_eox` drops the trailing `F7` → `#sysex{data = <<43,12,00,07>>}`,
  total bytes consumed `1+1+5 = 7`. The off-by-one is **structurally impossible** —
  there is no offset arithmetic to get wrong; it is sequential binary matching. The
  `sysex_multibyte_read_test` (300-byte payload, then a note-on whose correct read
  is the desync proof) is exactly the right witness.
- **#3 — track_end read/write shape mismatch — N/A this slice (read side clean).**
  End-of-track reads to `#meta_end_of_track{}` via the canonical vocabulary; the
  read/write shape *agreement* is a writer (slice B) + round-trip (slice C)
  concern. No shape divergence can exist on the read side alone. (Recorded so the
  Blocker is tracked to its closing slice, not assumed closed here.)
- **#2/S3 — seq_name written as EoT byte — deferred to slice B (write side).**

## 3. Disposition table (ledger rows)

Disposition: **Confirmed** / **Confirmed (execution = CC)** / **Finding**.

| # | Row | Basis | Disposition |
|---|-----|-------|-------------|
| 1 | `midi_codec` pure/framing-free | **grep**: `file:`/`io:`/`put(`/`get(` → only a doc-comment hit, none in code | Confirmed |
| 2 | core owns both directions, ±1, C5, no-vel-0-fold | **read**: `midi_codec_tests` covers each; C5 odd-value→`#control_change{}` (`controller_120_127_odd_value_is_cc_test`) and no-fold (`note_on_velocity_zero_not_folded_test`) are explicit | Confirmed |
| 3 | `midibin` refactor behaviour-preserving; Arc-3 suite unchanged | **git**: `git diff 06a6f65 f834f11 -- test/midibin_tests.erl test/prop_midibin.erl` → empty; **read**: `decode(<<Status,Data>>)` delegates, `{unknown,<<Status,Data>>}` reconstructs the exact bytes | Confirmed (execution = CC) |
| 4 | `read/1` typed return, no untagged `{Path,_}` | **grep**: `{Path` → none; `-spec` present; `read_open_failure_test` | Confirmed |
| 5 | **#1/S2** multi-byte SysEx, F0 accounted | **read**: traced above + `sysex_multibyte_read_test` | Confirmed |
| 6 | **#4/S4** seq-specific ≠ track-name | **read**: `decode_meta(0x7F)`→`#meta_sequencer_specific{}`, `0x03`→`#meta_track_name{}`; `*_distinct_*_test` | Confirmed |
| 7 | **S7** unterminated VLQ → `{error,{bad_vlq,_}}` | **read**: `read_vlq/4` errors at the 4th continuation byte and on byte-exhaustion; `bad_vlq_read_test` | Confirmed |
| 8 | **M2** no MThd → `{not_midi_file,_}` | **read**: `parse/2` fallback + `Format > 2` guard; `not_midi_file_test` | Confirmed |
| 9 | **#7/S8/C8** vel-0 faithful | **read**: `note_on` clause preserves `Vel`; `note_on_vel0_faithful_read_test` | Confirmed |
| 10 | **#13/C7** 1-based channels | **read**: `chan/1`=`(S band 16#0F)+1`; `channel_base_read_test` (nibble 0→1, 15→16) | Confirmed |
| 11 | **M1** canonical records only | **grep**: legacy tags → none; `canonical_records_only_test` | Confirmed |
| 12 | **#18** no process dict; state threaded, reset per track | **grep**: `put(`/`get(` → none; `running_status_reset_per_track_test` | Confirmed |
| 13 | **#22** no per-byte scan | **grep**: `look_for_chunk`/`pread` → only doc-comment; `read/1` = `file:read_file/1` + in-memory parse | Confirmed |
| 14 | **#26** `?DPRINT` gone | **grep**: none | Confirmed |
| 15 | division typed | **read**: `decode_division/2`; `256-Hi` for SMPTE fps (`0xE7`→25 ✓); `ppqn`/`smpte` tests | Confirmed |
| 16 | format 0 & 1, uniform track list | **read**: `#seq{tracks=[...]}`, no conductor field; `format0/1_read_test` | Confirmed |
| 17 | one fixture per event family | **read**: `channel_voice_families_test` (7), `channel_mode_family_test`, `meta_families_test` (14), `end_of_track_family_test` | Confirmed |
| 18 | running-status run | **read**: `running_status_read_test` (status elided on events 2–3) | Confirmed |
| 19 | eunit green (107) | CC: `rebar3 eunit` → All 107 passed | Confirmed (execution = CC) |
| 20 | `rebar3 check` green; dialyzer clean | CC: exit 0 / 0 warnings | Confirmed (execution = CC) |

**All 20 rows: confirmed.** 16 reproduced independently (grep/git/read); 3 rest on
CC's OTP-28 run (named gap); row 3 independently confirmed by git diff across the
slice.

## 4. CDC findings (beyond CC's floor)

- **F-CDC-1 — alien (non-`MTrk`) chunks crash rather than skip. Severity: Low
  (spec-robustness).** `parse_tracks/3` matches `<<"MTrk", …>>` and has no clause
  for a non-`MTrk` chunk while tracks remain, so it crashes (`function_clause`).
  CC discloses this as a deliberate let-it-crash (closing-report decision 4). But
  an *unknown* chunk is not malformed input — RP-001 (SMF spec) explicitly says a
  reader should "expect alien chunks and treat them as if they weren't there." The
  old `look_for_chunk` skipped them by scanning for the cookie; the rewrite does
  not, so a spec-legal file with an alien chunk now fails where it previously read.
  This is distinct from crashing on genuinely malformed bytes (which R4 sanctions).
  *Disposition:* **CLOSED** by follow-up commit `5065484`. `parse_tracks/3` now
  has a chunk-skip clause (after the `MTrk` clause, FP-12) that skips any non-`MTrk`
  chunk by its declared length without decrementing the track count (`ntrks`
  counts `MTrk` only). Re-verified independently: the one-clause diff and its four
  inline fixtures (`alien_chunk_before_first_track_test`,
  `alien_chunk_between_tracks_test`, `alien_chunk_trailing_ignored_test`,
  `truncated_chunk_still_crashes_test`) were read; the skip is correct and the
  truncated-chunk case correctly **still crashes** (`?assertError`). The fix touched
  only `src/midifile.erl` + `test/midifile_tests.erl`; `midi_codec`/`midibin`/
  headers/`midierrs` untouched (`git show --stat 5065484` confirms).

- **N-CDC-1 — encode-side error stance is inconsistent (forward note for slice B,
  not a slice-A defect).** `midi_codec:encode_meta/1` returns a structured
  `{error,{bad_value,denominator,_}}` for a bad time-sig denominator, but an
  out-of-range *other* modelled field (e.g. `#meta_sequence_number{value > 65535}`)
  has no clause and crashes. Defensible (out-of-range record = caller bug, EH-05),
  but slice B (writer) should decide one stance and apply it uniformly. Flagged so
  it is on the record before the writer leans on `encode_meta/1`.

## 5. Process / housekeeping notes

- **P-1 — the work is already committed.** CC's chat message ("Nothing is
  committed — say the word and I'll commit") is **stale**: `git log` shows three
  slice-A commits on `release/0.6.x` (`ee141bc`, `e69d8d3`, `f834f11`), and the
  closing report lives in `f834f11`. Nothing further to commit for the code; the
  ledger-CDC update + this document are the only new artifacts.
- **P-2 — stale `.git/index.lock`.** A 0-byte `.git/index.lock` is present and not
  removable from the CDC sandbox (permissions). Harmless for reads; may block git
  writes from some environments. Recommend `rm -f .git/index.lock` on the
  maintainer's machine if a git operation reports a lock.
- **P-3 — coverage-alias measurement gap (CC-disclosed, confirmed reasonable).**
  `rebar3 check` reports proper-only coverage; real combined coverage is
  `midi_codec` 97% / `midibin` 93% / `midifile` 81%, the uncovered `midifile` lines
  being the let-it-crash defensive paths. The enforced floor is slice C's concern
  (per `rebar.config` / Makefile). No action for slice A.

## 6. Trending

No recurring or systemic finding across slice A. The audit's cluster B
(process-dictionary state) and cluster C (untested `midifile`) are both *reversed*
on the read side: state is threaded, and `midifile` now has a real suite. The
single new finding (F-CDC-1) is isolated, not a trend.

## 7. Closure

- **Ledger:** all 21 rows verified (row 21 added by the follow-up); CDC column
  marked accordingly (rows 3/19/20 noted as execution-by-CC).
- **Findings:** **F-CDC-1 CLOSED** (follow-up commit `5065484`, re-verified — §4).
  **N-CDC-1 open**, carried to slice B (encode error stance). P-1/P-2/P-3 are
  process/housekeeping (P-1 superseded: all slice-A work is committed through
  `5065484`).
- **Follow-up nuance check (asked by the maintainer):** the follow-up did **not**
  over-apply let-it-crash. The two cases CC kept as crashes — a chunk whose
  declared length overruns the buffer, and a header declaring more `MTrk` chunks
  than exist — are *genuinely malformed* (no spec sanctions them), which is the
  correct side of the spec-legal/malformed line that F-CDC-1 was about. A separate,
  legitimate *contract* question remains for the maintainer (not a CC defect):
  whether file-level corruption (truncation, over-declared `ntrks`, zero division)
  should be promoted from a crash to a structured `{error, _}` value, since for a
  file reader these are foreseeable I/O corruption rather than programmer bugs.
  The current contract (R4: malformed binary may crash) is followed faithfully; the
  promotion is a deliberate choice to make or decline, best decided alongside the
  writer's error stance (N-CDC-1) in slice B.
- **Recommendation:** Slice A is **closed**. Proceed to author slice B (writer)
  cc-prompt + ledger; fold N-CDC-1 and the corruption-as-value contract question
  into its scope (or explicitly defer with rationale).
