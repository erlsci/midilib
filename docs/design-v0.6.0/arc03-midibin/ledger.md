# Arc 3 — `midibin` · ledger

Grep/test-verifiable acceptance rows. **CC** closes each with evidence (the
command run + its result); **CDC** re-verifies independently on close. Five-
iteration cap. A row that can't be met is surfaced as a disclosed deferral, never
silently dropped.

| # | Acceptance criterion | Evidence to cite | CC | CDC |
|---|----------------------|------------------|----|-----|
| 1 | `midibin` compiles clean on the project OTP floor (no OTP-27-only syntax unless row 12 bumps the floor) | `rebar3 compile` output | ☑ | ☐ |
| 2 | `decode/1` returns `{ok, message()}` / `{error,{unknown,_}}` / `{error,non_midi}` — no `{midi,_}` envelope, no bare `{unknown,Bin}` | grep clauses + eunit | ☑ | ☐ |
| 3 | `encode/1` takes bare records, returns `{ok,binary()}` / `{error, midierrs:reason()}` | grep clauses + eunit | ☑ | ☐ |
| 4 | Multi-byte SysEx round-trips: `decode(encode(#sysex{data=D})) =:= {ok,#sysex{data=D}}` for `byte_size(D) >= 256` | `sysex_multibyte_test` passes | ☑ | ☐ |
| 5 | `mono_mode_on` is implemented (round-trips, carries the count); no `not_implemented`/`?ERR_*` left in `midibin` | `mono_mode_on_test` + grep | ☑ | ☐ |
| 6 | `encode_batch/1` short-circuits with a single trailing `{error,_}`; `decode_batch/1` one result per element; no `Acc ++ [X]` | batch eunit tests + grep `++` | ☑ | ☐ |
| 7 | Controllers 120–127 at canonical values → `#channel_mode{}`; non-canonical value → `#control_change{}` (C5 policy) | `channel_mode_odd_value_is_cc_test` + `channel_mode_roundtrip_test` | ☑ | ☐ |
| 8 | No `?ERR_*` / `errors.hrl` references; reasons are atoms/terms; human text only via `midierrs:format_error/1` | grep src+test | ☑ | ☐ |
| 9 | `decode/1` and `encode/1` cover the **same** message-type set (no asymmetry) | grep parity (see CDC static check) + PropEr | ☑ | ☐ |
| 10 | eunit suite green | `rebar3 eunit` output | ☑ | ☐ |
| 11 | PropEr `prop_roundtrip` + `prop_encode_starts_with_status_byte` pass | `rebar3 proper` output (≥ default numtests) | ☑ | ☐ |
| 12 | OTP-floor decision resolved & applied: either kept portable (≥ stated floor, no `-doc`) or bumped to 27+ with `-doc`/`-moduledoc` restored — README/`.app.src`/CI updated to match | the decision note + grep for `-doc` | ☑ | ☐ |
| 13 | `rebar3 check` (compile, xref, dialyzer, eunit, coverage) green; dialyzer clean on `midibin` (specs use `message()`/`reason()`) | `rebar3 check` output | ☑ | ☐ |

## Notes for the closer
- Row 9 is the one the CDC draft already checked statically (decode↔encode set
  parity held); CC should confirm it still holds after any changes.
- Rows 1, 10, 11, 13 are the ones the CDC draft could **not** run (no Erlang
  toolchain in the authoring env) — they are the highest-value independent checks.
- Row 12 needs a human decision (see cc-prompt §OTP floor); CC should not bump the
  floor unilaterally.
