# Arc 4 · Slice C — round-trip + coverage · ledger

Grep/test-verifiable acceptance rows. **CC** closes each with evidence; **CDC**
re-verifies independently. Five-iteration cap. A row that can't be met is a
disclosed deferral, never a silent drop. This slice **closes Arc 4**.

| # | Acceptance criterion | Evidence to cite | CC | CDC |
|---|----------------------|------------------|----|-----|
| 1 | `prop_roundtrip` exists: `from_binary(to_binary(Seq)) =:= {ok, Seq}` over a `seq()` generator; runs under `rebar3 proper` (file `prop_*` so auto-discovery finds it) | `test/prop_midifile.erl`; `rebar3 as test proper` shows it run | ☐ | ☐ |
| 2 | The generator emits only round-trippable file-domain values: `control_change` control ∈ 0..119; `channel_mode` value 0 except `mono_mode_on`; time-sig denom a power of two; text payloads binary; `meta_unknown` type unmodelled-only; no system-common/real-time; tracks end in `#meta_end_of_track{}` | generator source review + `prop_roundtrip` passes ≥ default numtests (e.g. 1000) with no counterexample | ☐ | ☐ |
| 3 | `prop_roundtrip` passes at the agreed numtests with **no** shrunk counterexample | `rebar3 as test proper -n 1000` → property passed | ☐ | ☐ |
| 4 | Pure `from_binary/1` added (`-spec`, `{ok,#seq{}}|{error,reason()}`); `read/1` = `file:read_file/1` + `from_binary/1` | `-spec` present; `read/1` body shows the split | ☐ | ☐ |
| 5 | **Format 0, 1, 2 each** round-trip: `read(write(Seq)) =:= Seq` (fixed fixtures), confirming #6/S5 preservation end-to-end | `format0/1/2_roundtrip_test` pass | ☐ | ☐ |
| 6 | **Corruption-as-value** (if approved): a chunk overrunning the buffer → `{error,{truncated,_}}`; over-declared `ntrks` → `{error,{missing_tracks,_,_}}`; zero/invalid division → `{error,{bad_division,_}}`; truncated event/meta/sysex → `{error,{truncated,_}}`; dangling data byte → `{error,{bad_running_status,_}}` | one negative test per reason, each `{error,_}` (no `?assertError`) | ☐ | ☐ |
| 7 | New reasons added to `midierrs:reason()` + `format_error/1`; `midierrs` `-doc`/`-moduledoc` lines untouched | `git diff src/midierrs.erl` shows only reason/clause additions; `grep -nE '^-doc\|^-moduledoc' src/midierrs.erl` unchanged | ☐ | ☐ |
| 8 | The slice-A `truncated_chunk_still_crashes_test` is updated to the new `{error,_}` contract and renamed; the change is disclosed in the closing report | `git diff test/midifile_tests.erl`; closing-report note | ☐ | ☐ |
| 9 | **M4:** the coverage measurement aggregates eunit **and** proper (not proper-only); an eunit-only-exercised line shows covered | coverage report before/after; an eunit-only line's hit count > 0 | ☐ | ☐ |
| 10 | **M4:** the coverage floor is raised from 0 to the agreed threshold and enforced by `make check` / CI (a drop below fails the build) | `rebar.config`/`Makefile`/CI diff; a deliberate under-threshold run fails | ☐ | ☐ |
| 11 | All prior suites stay green: slice-A reader, slice-B writer, Arc-3 `midibin`, `midi_codec` | `rebar3 eunit` all pass; `git diff` on Arc-3 `midibin` test files empty | ☐ | ☐ |
| 12 | `rebar3 check` green with the raised floor; `rebar3 proper` green; dialyzer clean across all four modules | `rebar3 as test check` → exit 0; `dialyzer` → 0 warnings | ☐ | ☐ |
| 13 | **Arc-4 reconciliation:** every RELEASE-PLAN row-4 finding shown closed in slice A, B, or C (no silent drops) | a table in the closing report mapping each finding → its slice + evidence | ☐ | ☐ |

## Notes for the closer
- Row 2 is the subtle one: a generator that emits a non-round-trippable value
  (e.g. `#control_change{control=123}`, a `chardata` *list* text, a `meta_unknown`
  with a modelled type) makes `prop_roundtrip` fail for the *wrong* reason. Each
  restriction is correctness — annotate the generator with the *why*.
- Row 6 depends on the maintainer approving corruption-as-value (cc-prompt
  § corruption-as-value). If declined, this row instead asserts the *current* crash
  behaviour and the closing report records the decision.
- Row 13 is the arc gate: Arc 4 is not done until every finding it owns is shown
  closed across the three slices.
- Run-only rows (3, 9–12) are the toolchain-dependent ones; name the gap if the
  authoring env lacks `rebar3`, first run is the close-out (as in slices A/B).
