# Arc 3 — `midibin` · closing report (CC)

**Release:** v0.6.0 · **Arc:** 3 of 6 · **Date:** 2026.06.17
**Toolchain used:** Erlang/OTP 28, rebar3 3.27.0 (the CDC draft was authored with **no**
toolchain; this close is the first time the code was compiled, eunit-run, dialyzed,
and PropEr-run).
**Result:** all 13 ledger rows closed with evidence; `rebar3 as test check` green
end-to-end (exit 0).

## Headline

The CDC draft of `src/midibin.erl`, `test/midibin_tests.erl`, and
`test/midibin_props.erl` was validated and finalized onto the canonical
vocabulary (`include/midi_msg.hrl`) and the `{ok,_}|{error,_}` contract. The draft
was substantially correct; finalizing it required four real changes (below), the
OTP-floor decision (resolved with the maintainer), and two tooling/scope fixes to
make `check` genuinely green. No ledger row was deferred.

## Changes I made to the draft (I own the result)

1. **`mode_control/2` dead-code / dialyzer fix.** The draft's `mode_control(_, _) -> error`
   clause and the `error ->` branch in the `#channel_mode{}` encode clause were
   unreachable — `channel_mode_name()` is a closed union, so dialyzer proved the
   `error` path impossible (2 warnings). Removed both; an out-of-contract mode atom
   now crashes (EH-05, "crash on bugs"), which is correct for a type violation.
   `src/midibin.erl:135-219`.
2. **`decode/1` reconciled to the design of record.** The draft (following the
   arc-plan signature) had `decode/1` return `{error, non_midi}` for a non-binary
   argument. **DESIGN-vocabulary §6 scopes `decode/1` to `{ok, message()} |
   {error, {unknown, binary()}}`** — `non_midi` is an *encode-only* reason. Dialyzer
   flagged `decode('not_a_binary')` as a contract break. Aligned the spec and code to
   §6: `decode/1` takes `binary()`, unrecognised bytes are `{error,{unknown,Bin}}`,
   and a non-binary argument is a caller bug that crashes. Updated `decode_unknown_test`
   accordingly. `src/midibin.erl:30-104`, `test/midibin_tests.erl:172-178`.
3. **PropEr module renamed `midibin_props` → `prop_midibin`.** rebar3_proper's
   auto-discovery (`rebar3_proper_prv.erl:312`) only picks up files whose basename
   starts with `prop_`; under the original name the bare `proper` command the
   `coverage`/`check` alias invokes found **0 properties**. Renaming makes
   `rebar3 as test check` actually run the properties. The property *function* names
   the ledger cites (`prop_roundtrip`, `prop_encode_starts_with_status_byte`) are
   unchanged. `test/prop_midibin.erl`.
4. **`batch_short_circuits_on_error_test` rewritten to a typed trigger.** The draft
   used a bare atom `not_a_message` as the bad element, violating `encode_batch`'s
   `[message()]` spec (dialyzer "no local return"). Replaced with a well-typed
   `message()` the wire codec legitimately rejects — a `#meta_set_tempo{}` (meta is
   file-only) — which is the realistic R9 short-circuit trigger and keeps the precise
   spec. `test/midibin_tests.erl:158-169`.

## Decisions resolved with the maintainer

- **OTP floor (row 12): support OTP 22–29, kept portable.** No `-doc`/`-moduledoc`
  (those are OTP-27+ only). Applied: README erlang badge `21–27 → 22–29`; CI matrix
  `core-builds` now `['29','28','27','26','25','24']` with `rebar3 3.27`, `older-builds`
  now `['23','22']` (dropped 21); `midibin` module comment updated to state the
  resolved floor. (The workflow was renamed `cicd.yml → ci.yml`; I also repointed the
  README build badge to the new `ci` workflow name.)
- **`midimsg:bank_select/3` spec (greening `check`): 1-line fix approved.** Its `-spec`
  said `-> list()` but it returns a `{midi,{batch,_}}` tuple (the last remaining
  dialyzer warning, pre-existing/Arc-5). Corrected to `-> tuple()` with maintainer
  approval — a disclosed, scoped exception, not Arc-5 rewiring. `src/midimsg.erl:359`.

## Tooling change

- **`rebar.config` dialyzer `plt_extra_apps`.** With `{warnings,[unknown]}` enabled,
  dialyzer reported false "unknown function" hits for `proper_types`, `proper`,
  `eunit`, and `uuid` (apps outside the base PLT). Added
  `{plt_extra_apps, [uuid, eunit, proper]}` so dialyzer can see them. This dropped the
  test-profile warning count from 23 → 1 (then 0 after the `midimsg` spec fix).

## Ledger walk (evidence)

| # | Criterion | Status | Evidence |
|---|-----------|--------|----------|
| 1 | Compiles clean on the floor; no OTP-27-only syntax | ✅ | `rebar3 compile` clean; `grep -n '^-doc\|^-moduledoc' src/midibin.erl` → none |
| 2 | `decode/1` → `{ok,message()}` / `{error,{unknown,_}}`; no `{midi,_}`, no bare `{unknown,Bin}` | ✅ | 28 `{ok, #…}` decode clauses; no `{midi,` outside comments; `decode_unknown_test`, all round-trips pass. **Reconciled to DESIGN §6** (no `non_midi` from decode — see change #2) |
| 3 | `encode/1` takes bare records → `{ok,binary()}` / `{error, midierrs:reason()}` | ✅ | `encode_*` eunit tests; `encode_out_of_range_test`, `encode_meta_is_unsupported_test`, `encode_non_message_test` pass |
| 4 | Multi-byte SysEx round-trips, `byte_size(D) >= 256` | ✅ | `sysex_multibyte_test` (300-byte payload) passes; also `sysex_empty_payload_test`, `sysex_truncated_is_unknown_test` |
| 5 | `mono_mode_on` implemented; no `not_implemented`/`?ERR_*` in `midibin` | ✅ | `mono_mode_on_test` passes; `grep not_implemented src/midibin.erl` → none |
| 6 | `encode_batch/1` short-circuits with one trailing `{error,_}`; `decode_batch/1` 1:1; no `Acc ++ [X]` | ✅ | `batch_*` eunit tests; `grep '++' src/midibin.erl` → none (reversed accumulator + list comprehension) |
| 7 | Controllers 120–127 canonical → `#channel_mode{}`; non-canonical value → `#control_change{}` | ✅ | `channel_mode_roundtrip_test`, `channel_mode_odd_value_is_cc_test` pass |
| 8 | No `?ERR_*` / `errors.hrl` refs; reasons are atoms/terms; human text only via `midierrs:format_error/1` | ✅ | `grep 'ERR_\|errors.hrl' src/midibin.erl test/*.erl` → none. `include/errors.hrl` deleted (by maintainer) |
| 9 | `decode/1` and `encode/1` cover the **same** message set | ✅ | Static parity: every record `decode/1` emits has an `encode/1` clause and vice versa; `prop_roundtrip` (1000) exercises the union |
| 10 | eunit green | ✅ | `rebar3 eunit` → **All 28 tests passed** |
| 11 | PropEr `prop_roundtrip` + `prop_encode_starts_with_status_byte` pass | ✅ | `rebar3 as test proper -n 1000` → **2/2 properties passed**, 1000 each |
| 12 | OTP-floor resolved & applied | ✅ | Kept portable, floor OTP 22–29; README badge + `ci.yml` matrix + module comment updated; no `-doc` in `midibin` |
| 13 | `rebar3 check` green; dialyzer clean on `midibin` | ✅ | `rebar3 as test check` → **exit 0**; `rebar3 as test dialyzer` → 0 warnings |

## Verification commands (reproducible)

```
rebar3 compile                       # clean
rebar3 xref                          # clean
rebar3 eunit                         # All 28 tests passed
rebar3 as test proper -n 1000        # 2/2 properties passed (1000 each)
rebar3 as test dialyzer              # 0 warnings
rebar3 as test check                 # exit 0 (compile, xref, dialyzer, eunit, coverage)
```

## Notes for CDC verification

- **Coverage characteristic:** the `coverage` alias runs `proper -c` then `cover -v`,
  so the reported figure (midibin **81%**) is *property-only* coverage; eunit's cover
  is not aggregated into it. No floor is enforced here (per the arc-02 rebar.config
  note); the Makefile/CI owns the threshold. Combined eunit+proper coverage of
  `midibin` is higher than 81%.
- **OTP-29 CI pairing is declared but not executed here.** I verified locally on OTP 28
  / rebar3 3.27. The `ci.yml` `core-builds` matrix now declares 24–29 with rebar3 3.27;
  the OTP-29 + rebar3-3.27 combination is asserted by the maintainer's floor decision,
  not run in this environment.
- **Two out-of-scope touches, both disclosed/approved:** `midimsg:bank_select/3` spec
  (maintainer-approved 1-liner) and `rebar.config` `plt_extra_apps` (tooling). `midifile`,
  `midiutil`, the `#event{}`/`#seq{}` containers, and `midimsg`'s vocabulary rewiring
  remain untouched (Arcs 4–5).
- The `include/errors.hrl` deletion landed (maintainer-owned, per the prompt); `check`
  is green with it gone, confirming nothing in the build still references it.
