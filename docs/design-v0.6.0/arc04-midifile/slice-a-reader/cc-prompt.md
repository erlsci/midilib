# CC assignment — Arc 4 · Slice A: `midi_codec` + `midibin` refactor + `midifile:read/1`

> The assignment the implementing context (CC) receives. Self-contained. Read the
> reference docs first, then implement to the ledger. **No draft exists in the
> tree this time** — you author `midi_codec`, the `midibin` refactor, the
> `midifile:read/1` rewrite, and the reader suite from scratch, against the design.
> CDC verifies independently on close.

## Posture

Peer-frame, write-to-the-floor. Load the **collaboration-framework** and
**erlang-guidelines** skills (`11-anti-patterns.md` first; also `01-core-idioms.md`,
`02-api-design.md`, `03-error-handling.md`, `04-data-and-types.md`,
`05-functions-and-pattern-matching.md`, `15-testing.md`). The canonical vocabulary
is the contract — do not invent shapes; use the records in `include/midi_msg.hrl`
verbatim and the reasons in `src/midierrs.erl`.

This is the first slice of the `midifile` rewrite and it stands up a **new shared
module, `midi_codec`**, that both codecs sit on. Get its byte mapping right and
the read/write drift the audit found cannot recur.

## Required reading (evidence, not summaries)

1. `docs/design-v0.6.0/arc04-midifile/arc-plan.md` — the three-layer architecture
   (`midi_codec` / `midibin` / `midifile`), the slice breakdown, the vocabulary
   adoption and error contract, the OTP/docs stance.
2. `docs/design-v0.6.0/arc04-midifile/slice-a-reader/arc-plan.md` — this slice's
   design: the `midi_codec` API sketch, the `read/1` design, the fixtures.
3. `docs/design-v0.6.0/arc04-midifile/slice-a-reader/ledger.md` — the acceptance
   rows you close with evidence.
4. `docs/design-v0.6.0/arc02-canonical-vocabulary/DESIGN-vocabulary.md` — the
   record catalogue (§4), the tag migration (§5), the error contract (§6), the
   normalization stance (§3.5), the channel base (§3.3), the container shapes and
   the `division`/conductor-track changes (§9).
5. `include/midi_msg.hrl`, `include/midi.hrl`, `src/midierrs.erl` — the vocabulary,
   the wire/file constants, and the error reasons. Use them; don't redefine.
6. `src/midibin.erl` and its tests — the Arc-3 codec you are refactoring (and the
   suite that must stay green) and the idiom reference for clause ordering,
   guards, and the `±1` channel mapping.
7. Audit context for the findings this slice closes:
   `…/arc00-audit/2026.06.17-audit-results-erlang.md` (#1, #4, #7, #13, #15, #18,
   #19, #22, #26), `…-midi-spec.md` (S2, S4, S7, S8), `…-consumer-fit.md` (C7, C8),
   and the CDC eval `…-audit-eval-cdc.md` (M1, M2; cluster B/C → rewrite, not spot
   fixes).

## What to build

1. **`src/midi_codec.erl`** — pure, stateless, framing-free message↔bytes core,
   **both directions** (`decode_message/2`, `encode_message/1`, `data_length/1`,
   `decode_meta/2`, `encode_meta/1` — refine signatures as the design demands).
   It owns: the 1-based channel `±1`, the C5 channel-mode policy, the no-vel-0-fold
   rule, and the meta value conversions (tempo µs/qn, time-sig `2^dd`, key-sig,
   `meta_unknown` verbatim). No I/O, no running status, no delta-time, no file
   concepts.
2. **`src/midibin.erl`** — refactor to delegate the body mapping to `midi_codec`,
   keeping its public API and Arc-3 behaviour **exactly**. Wire framing (single
   real-time bytes, `F0…F7` SysEx, one-message-per-call) stays in `midibin`.
3. **`src/midifile.erl`** — `read/1` only (leave `write/2` for slice B; if the old
   `write/2` would break compilation against the new shapes, stub it to
   `{error, not_implemented}` with a `% slice B` note rather than carrying dead
   legacy code).
4. **`test/midifile_tests.erl`** + inline-binary fixtures (see slice plan): the
   reader's evidence for #1/S2, #4/S4, S7, M2, plus each event family, running
   status, SMPTE division, and format 0.

## Specifically re-derive, don't trust

Every bit-syntax pattern: the MThd/MTrk chunk layout, the VLQ reader (including
the S7 unterminated-4-byte rejection), the SysEx `F0 <vlq-len> <…F7>` framing and
its byte accounting (the #1/S2 fix — payload at `pos + 1 + len_bytes`, consume
`1 + len_bytes + Length`), the running-status recovery and `data_length` table,
the meta value conversions, and the SMPTE `division` decode. The slice-plan API
sketch is a starting point you own, not gospel.

## Constraints (erlang-guidelines)

- snake_case; `-spec` every exported function using `message()` / `event()` /
  `midierrs:reason()` (no `-> tuple()`); `{ok,_}`/`{error,_}` returns.
- **No process dictionary.** Thread `{Status, Chan}` as accumulator arguments,
  reset per track (#18, PC-12). This is the cluster-B fix — do not reintroduce
  `put`/`get`.
- **No silent truncation / no stringly errors.** Predictable failures are
  `{error, midierrs:reason()}` values; `exit("string" ++ Int)` is gone (#5/#17 is
  slice B, but don't reintroduce the idiom anywhere). Genuinely malformed binary
  may crash (let-it-crash, R4) — but missing `MThd` and bad VLQ are *values*.
- **Faithful, not normalizing:** Note-On vel 0 stays `#note_on{velocity = 0}`
  (C8/R6); never invent release velocity (#7/S8).
- **One source of truth:** channel `±1`, the C5 policy, and the byte layouts live
  in `midi_codec` only — `midibin` and `midifile` call it, they don't re-implement.
- **Keep portable (OTP 22–29):** `%%` comments + `-spec`, **no** `-doc`/
  `-moduledoc` (match `midibin`). Do not touch `midierrs`'s existing `-doc` (Arc 6).
- Clause ordering (FP-12): specific channel-mode clauses before the generic CC
  clause; confirm nothing is shadowed.

## Out of scope (do not build)

`midifile:write/2` (slice B); the round-trip PropEr property + coverage floor
(slice C); `midimsg`/`midiutil` (Arc 5); the `midierrs` `-doc` reconciliation
(Arc 6). Don't expand `midibin`'s public API. Don't change `midi_msg.hrl` or
`midierrs.erl` (if you believe a vocabulary record is wrong, **stop and surface
it** — it's a design change, not a slice edit).

## Done

Every ledger row closed with cited evidence; `rebar3 check` green (compile, xref,
dialyzer, eunit, coverage) and `rebar3 proper` green on the OTP floor; the **Arc-3
`midibin` suite still green unchanged** (the refactor's proof). Write
`closing-report.md` (per-row walk with evidence, plus any change you made and own,
any decision that needed the maintainer, and any disclosed deferral). If a row
can't be met, stop and surface it — don't silently drop it. Five-iteration cap.
CDC then writes `cdc-verification.md` independently.
