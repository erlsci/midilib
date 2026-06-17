# Audit prompt — midilib code-quality, correctness, Erlang practice & MIDI-spec adherence

> Paste this as the opening message of a fresh session. It is self-contained;
> assume you remember nothing of prior conversations. This is the
> collaboration-framework `CODE-AUDIT.md` working-practice prompt, **specialized
> for one library (`midilib`)** and extended with two dimensions the generic
> template does not cover: **MIDI 1.0 spec adherence** and **fit for the
> downstream consumers** (`midi`, `midiio`, `undermidi`).
>
> **Diagnosis only. Do not modify, stage, or commit any source.** A separate
> fix round follows. Your output is reports.

## 0. How to start

1. Load the **collaboration-framework** skill; read `AI-CONSTITUTION-SUPPLEMENT.md`
   and `AI-ENGINEERING-METHODOLOGY.md`. You are the *doer* in a CAP-style audit:
   independent, evidence-based, severity-classified, closure-tracked. Write to
   the floor; cite evidence; do not soft-pedal.
2. Load the **erlang-guidelines** skill. Read `11-anti-patterns.md` **first** —
   it is the canonical hunt list. Then read the chapters this codebase actually
   exercises: `01-core-idioms`, `02-api-design`, `03-error-handling`,
   `04-data-and-types`, `05-functions-and-pattern-matching`, `13-documentation`,
   `15-testing`, `17-tooling`. (midilib is a pure library — no processes, OTP
   behaviours, or supervision — so `06`/`07`/`08` are largely N/A, **except**
   that `midifile` keeps mutable state in the process dictionary; judge that
   against `06`/anti-patterns.)
3. Read the evidence in §3–§5 before writing any finding.
4. The guides are the **contract**. Do not fall back to generic Erlang
   knowledge where a guide has an opinion; cite the pattern ID (e.g. `EH-03`,
   `AP-12`) when a finding rests on one.

## 1. Preparation (from the template)

1. Run `date +%Y.%m.%d`; capture as `<DATE>`. Do not hallucinate the date.
2. Read project context: `README.md` (root), `CLAUDE.md` if present, and any
   design/architecture doc they reference. **There is no architecture or design
   document in this repo** — per the template, record that gap in the executive
   summary of each report. The closest things to design intent are the family
   bootstrap docs named in §6 (consumer dimension); they are not midilib design
   docs.
3. Language detection: Erlang only (`.erl`, `.hrl`, `rebar.config`). One
   language ⇒ the language report is `erlang`.

## 2. The four audit dimensions

Audit midilib along **four** axes. A finding may belong to more than one; file
it where its root cause lives and cross-reference.

1. **Code quality & Erlang best practice** — against the erlang-guidelines
   guides; the anti-patterns chapter is the starting hunt list.
2. **Conceptual correctness (MIDI semantics)** — independent of the spec text:
   running status, note-on velocity 0 ⇒ note-off, 14-bit values (pitch bend,
   song-position), variable-length quantities (VLQ) for delta-times, SysEx
   framing and the `0xF7` terminator / `0xF7` escape, meta-event structure,
   channel 1-vs-0 indexing, and the **dual meaning of `0xFF`** (System Reset on
   the wire vs. Meta-event prefix in a file).
3. **MIDI 1.0 spec adherence** — against the authoritative spec PDFs in
   `priv/docs/` (§3). A spec-adherence finding **must cite the governing PDF and
   section/page**; an uncited spec claim is not admissible.
4. **Consumer fit** — audit the *public API* as the downstream layers will
   actually call it (§6). midilib is the codec the whole erlsci MIDI family
   routes through; an API that is internally consistent but wrong for the bridge
   is still a finding.

## 3. The spec corpus (authoritative — in `priv/docs/`)

Extract text from the relevant PDF (use `pdftotext`, or the `pdf` skill) and
cite section/page. Which spec governs what:

| PDF (in `priv/docs/`) | Governs in midilib |
|---|---|
| **MIDI 1.0 Detailed Spec** (`M1_v4-2-1…`) | Channel/system/realtime message bytes, status nibbles, 14-bit fields → `midibin.erl`, `midimsg.erl`, `include/midi.hrl` |
| **Standard MIDI Files** (`RP-001…`) | SMF chunk layout (`MThd`/`MTrk`), header format word, VLQ, running status, meta events, format 0/1/2 → `midifile.erl` |
| **MIDI Time Code** (`RP-004-008…`) | MTC quarter-frame & SMPTE offset → `time_code_quarter_frame`, `smpte` |
| **General MIDI 1 / 2** (`RP-003…`, `General_MIDI_Level_2…`) | program/bank semantics, controller numbers → `bank_select`, `program_change`, `cc` |
| **SMF Lyric (rp17) / Device & Program Name (rp19)** | meta-event payloads for lyric/device/program name → `midifile` meta handling, `midimsg:lyric/device/program` |
| **MIDI 2.0 & UMP**, **Bit Scaling** | **Out of scope.** midilib is a MIDI 1.0 library and does not claim 2.0. Do **not** grade midilib against 2.0; only flag any place 1.0/2.0 are *conflated*. |

## 4. Module map (orient yourself — then verify, don't trust this)

- `src/midibin.erl` (~451 LOC) — term⇄binary codec; public `encode/1`, `decode/1`.
- `src/midimsg.erl` (~395) — Erlang-term message constructors; its header claims
  to be the project-wide "lingua franca."
- `src/midifile.erl` (~382) — Standard MIDI File `read/1`, `write/2`. Origin: Jim
  Menard's midilib (~2010). Keeps running-status state in the **process dictionary**.
- `src/midiutil.erl` (~112) — note/beat/quantize/name helpers.
- `src/midilib.erl` (~11), `src/midilib_versions.erl` (~39) — version surface.
- `src/midierrs.erl` (1 line) — module declaration only; no exports.
- `include/midi.hrl` — status-byte and meta-event macros.
- `include/errors.hrl` — error-reason macros (string reasons).
- `test/midibin_tests.erl` — the **only** test module; covers `midibin` only.

## 5. Seed findings — a calibration set, NOT the scope

The following were found during planning analysis and verified by reading. Treat
them as a **floor**: confirm each against source (and spec where relevant),
assign a severity, and — most importantly — **find what is not on this list.**
The audit's value is in what you add. (Verify the *characterizations* too; the
analyst may be wrong.)

**Cross-module / consumer-critical**

- **Three overlapping, incompatible message vocabularies.** `midimsg` constructors,
  `midibin`'s codec terms, and `midifile`'s event tuples are different shapes,
  yet `midimsg.erl:11-16` and `README.md:31` claim one common form. E.g. `midifile`
  emits `{copyright, DeltaTime, Data}` while `midimsg:copyright/1` makes
  `{midi, {copyright, Text}}` and `midibin` has no clause for either. This
  directly undermines the family's "one message representation across realtime
  and files" goal (see §6).
- **SysEx is single-7-bit-byte only.** `midibin` decode `midibin.erl:137-138`
  and encode `midibin.erl:410-411` handle exactly one data byte; `midimsg:sys_ex/1`
  (`midimsg.erl:304-306`) accepts a `binary()` the codec cannot encode. The
  Detailed Spec defines SysEx as arbitrary-length. This is the #1 cross-layer
  blocker for the family (see `midi`'s R7 in §6).
- **`rt_tick` has no encoder.** `midimsg:rt_tick/0` (`midimsg.erl:296-298`) makes
  `{midi, {realtime, tick}}`, which `midibin:encode/1` cannot match → falls to
  `?ERR_MIDI_UNSUP`.

**`midifile` correctness (SMF)**

- **`seq_name` writes the wrong meta type.** `event_io_list({seq_name, …})` at
  `midifile.erl:319-321` emits `?META_TRACK_END` (`0x2F`) instead of
  `?META_SEQ_NAME` (`0x03`) → corrupt SMF. Likely Blocker.
- **`?META_SEQUENCER_SPECIFIC` reads as `{seq_name, …}`.** `midifile.erl:188` tags
  sequencer-specific data as `seq_name`, colliding with the real track-name event.
- **`track_end` read/write shape mismatch.** Read emits `{track_end, DeltaTime, []}`
  (`midifile.erl:147`, 3-tuple); the write clause matches `{track_end, DeltaTime}`
  (`midifile.erl:309`, 2-tuple) → writing a round-tripped `track_end` fails to match.
- **`write/2` hardcodes format 1** (`midifile.erl:247`, the `0, 1` header word)
  while `read/1` parses any format (`midifile.erl:51-53`). A format-0 or format-2
  file cannot be faithfully round-tripped, and the `seq` shape has no slot for the
  original format. Reconcile against `README.md:39` ("type 1 MIDI files").
- **Running-status reconstruction assumes 3-byte events.** `midifile.erl:201-207`
  rebuilds `<<Status:4, Chan:4, B0:8, B1:8>>` for any running-status byte, which
  mis-frames the 2-byte channel messages (program change, channel pressure).
- **VLQ overflow path crashes wrongly.** `var_len/1` at `midifile.erl:381-382`
  does `exit("Value " ++ I ++ " …")` with `I` an integer → `badarg` in `++`, not
  the intended diagnostic.
- **Mutable state in the process dictionary.** `put/get` of `status`/`chan`
  throughout `midifile.erl` (e.g. `:73-74`, `:202-203`, `:359-361`) — not
  reentrant, hidden, hard to test; judge against the anti-patterns chapter.
- **`read/1` error contract.** On open failure returns `{Path, Error}`
  (`midifile.erl:37-38`), not a tagged `{ok,_}|{error,_}`.

**Hygiene**

- **String error reasons.** `include/errors.hrl:1-3` uses `{error, "non-MIDI
  message"}` etc.; string reasons resist matching (prefer atoms, optionally with
  context). Note `midibin` depends on these via `errors.hrl`.
- **Dead module.** `src/midierrs.erl` is an empty `-module(midierrs).` with no
  code.
- **Sparse specs / no `-moduledoc`/`-doc`.** Most public functions lack `-spec`;
  no module docs (OTP-27 `-doc` available). Check coverage across all modules.

**Testing**

- **Coverage is `midibin`-only.** `midifile`, `midimsg`, `midiutil` have **no**
  tests — and `midifile` is the most defect-dense module. `sys_ex_test`
  (`test/midibin_tests.erl:224-227`) asserts a single-integer SysEx, baking the
  codec limitation into the suite. The `batch` test payload shape
  (`:233-243`) differs from what `midimsg:batch/2` (`midimsg.erl:104-106`)
  produces — confirm the two batch shapes are actually compatible.

## 6. Consumer dimension — audit the API as the family will call it

midilib sits under a transport layer and a glue layer:

```
minimidio.h ─► midiio (NIF transport, codec-free)     midilib (codec + .mid, pure Erlang)
                       └──────────────┬───────────────────────┘
                                  midi (glue facade) ─► undermidi (app)
```

`midi` will call `midibin:encode/1` on the outbound path and `midibin:decode/1`
on every inbound message, and will stream `midifile:read/1` output through the
same send path. Read these for what the consumer needs, then audit midilib's
public surface against it:

- `../../../midi/workbench/SESSION-BOOTSTRAP-api-planning.md` — the glue layer's scope.
- `../../../midi/workbench/NEEDS-from-midiio.md` — requirements on the boundary;
  **R7** is the SysEx codec gap (above), and R1/R6 bear on decode completeness
  and normalization ownership.
- `../../../midiio/workbench/SESSION-BOOTSTRAP-nif-planning.md` — the transport
  contract (one-complete-message-per-delivery, byte-exact passthrough).

Consumer-fit findings to grade: codec **completeness** (every message `decode/1`
can receive from a live device must round-trip; what returns `{unknown, Bin}`?),
**representation unity** (the three-vocabulary problem), **error contract**
(tagged, matchable — what `midi` must branch on), and **batch semantics**
(does `midibin`'s list-of-binaries batch match what `midimsg:batch` builds?).

## 7. Output

Run `date +%Y.%m.%d` first. Write Markdown to **`workbench/audit/`** (this dir):

- `workbench/audit/<DATE>-audit-results-erlang.md` — dimensions 1 & 2 (code
  quality, Erlang practice, conceptual correctness).
- `workbench/audit/<DATE>-audit-results-midi-spec.md` — dimension 3 (spec
  adherence; every finding cites a PDF section/page).
- `workbench/audit/<DATE>-audit-results-consumer-fit.md` — dimension 4 (API as
  consumed by `midi`/`midiio`; cross-reference `NEEDS-from-midiio.md`).
- `workbench/audit/<DATE>-audit-index.md` — the index.

**Per-report structure** (from the template): (1) executive summary, 3–5
sentences, noting the missing architecture doc; (2) findings grouped —
correctness/soundness, API design & invariants, error handling, concurrency/
runtime safety, testing, performance, idioms/style — highest severity first,
omit categories that do not apply but do not invent new ones; (3) per finding:
**Severity** (Blocker/High/Medium/Low), **Location** `file:line`, **What's
wrong**, **Why it's wrong** (the failure mode, not a restatement), **Fix**
(concrete, at the cited line, with a snippet if non-obvious); (4) cross-cutting
findings; (5) **"Things I looked for and did not find"** — at least five clean
checks per report, to discipline against padding.

**Index** (`<DATE>-audit-index.md`): date and project root; language detected
(Erlang, audited); per-report finding counts by severity with relative links;
the architecture-doc-discovery note (none found, where you looked); and any
cross-cutting findings linking back to their root-cause report. Add a short
**trend** note: the dominant root-cause clusters (the planning analysis suspects
two — the vocabulary split and `midifile`'s process-dictionary state machine;
confirm or refute).

## 8. Stance (from the template)

- **Do not soft-pedal.** A real bug reads "fix this," not "consider this." The
  only exception is a genuine open design tradeoff — label it "open question"
  and put it at the end of its category.
- **The current state of the code is not evidence it is correct.** It compiles
  and `midibin` tests pass; that proves only that the compiler and those tests
  are satisfied. Hunt what the tests do not cover — almost everything in
  `midifile`.
- **No generic advice.** Every recommendation lands on a `file:line`.
- **Severity is a commitment.** Do not use Medium as a hedge; if torn, write one
  line of reasoning and pick.
- **Spec claims cite the PDF.** No citation ⇒ move it to conceptual-correctness
  or drop it.
- **The seed set is a floor, not a ceiling.** An audit that only confirms §5 has
  failed; the value is in what you find beyond it.

## 9. Do not modify code

Diagnosis only. Do not stage, commit, or edit source files. The fix round is
separate.
