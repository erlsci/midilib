# Session bootstrap — evaluating CC's midilib audit (CDC frame)

> Paste this as the opening message of a fresh session. It is self-contained;
> assume you remember nothing of prior conversations. Its job is to put you —
> the independent evaluator (CDC) — into the same contextual frame the analyst
> who scoped this audit was working in, so you can judge CC's findings the way a
> peer who already knows the family would.
>
> **You are not re-running the audit and not rubber-stamping it.** You are the
> *independent* leg of a CAP-style corrective-action loop: verify CC's findings
> against the actual artifacts, classify each, catch what CC missed or
> overstated, and say whether the audit is solid enough to plan a fix round from.

## 0. How to start

1. Load the **collaboration-framework** skill; read `AI-CONSTITUTION-SUPPLEMENT.md`
   and `AI-ENGINEERING-METHODOLOGY.md`. The five CAP properties are your rubric:
   **independence** (you are not the doer — CC was), **evidence access** (you read
   the real files and PDFs, never CC's summary), **severity classification** (don't
   flatten the scale), **trending** (recurring root causes are systemic), and
   **closure discipline** (every finding gets a written disposition — "noted" is
   not one).
2. Load the **erlang-guidelines** skill (`11-anti-patterns.md` first) so you can
   judge whether CC's Erlang findings are real and whether it missed any. The
   guides are the contract; CC should cite pattern IDs — check the citation fits.
3. Have the MIDI spec PDFs in `midilib/priv/docs/` open as needed. When CC makes
   a spec-adherence claim, **open the cited section yourself and confirm it says
   what CC says.** Do not inherit CC's reading of the spec.
4. Read the inputs in §2 before evaluating. Peer frame, boldness default: report
   real assessments, name where you're uncertain, take compensatory action (open
   the PDF, run a round-trip in a `rebar3 shell`) rather than bluff.

## 1. The frame you're inheriting

**The family.** midilib is one repo in the erlsci MIDI family. Layering:

```
minimidio.h ─► midiio (NIF transport, codec-free)     midilib (codec + .mid, pure Erlang)
                       └──────────────┬───────────────────────┘
                                  midi (glue facade) ─► undermidi (app)
```

We are currently **planning** two of these — `midi` (the glue/facade) and
`midiio` (the NIF transport over the vendored single-header C lib `minimidio`).
midilib is the **codec the whole family routes through**: `midi` will call
`midibin:encode/1` outbound and `midibin:decode/1` on every inbound message, and
will stream `midifile:read/1` output through the same send path. That is *why*
this audit exists now and why it carries a consumer dimension — we are auditing
midilib as the layer above is about to lean on it hard.

**What the audit covers.** CC was asked to audit midilib on four axes: (1) code
quality & Erlang best practice, (2) conceptual MIDI correctness, (3) MIDI 1.0
spec adherence against the PDFs in `priv/docs/`, and (4) fit for the downstream
consumers. The full assignment is in `workbench/audit/AUDIT-PROMPT-cc.md` — read
it; CC's output should be judged against it.

**The consumer context (read these).** They define what "fit for consumer" means
and contain the cross-layer story:

- `../../../midi/workbench/NEEDS-from-midiio.md` — requirements on the transport
  boundary. **R7** is the midilib SysEx codec gap; R1/R6 bear on decode
  completeness and where note-on-vel-0 normalization lives. The midilib SysEx
  single-byte limit is the #1 cross-layer blocker for the family.
- `../../../midi/workbench/UPSTREAM-minimidio.md` — minimidio findings (U1–U3);
  U2 (vel-0 fold) and U3 (real-time-in-SysEx) interact with how midilib must
  decode and normalize.
- `../../../midi/workbench/SESSION-BOOTSTRAP-api-planning.md` and
  `../../../midiio/workbench/SESSION-BOOTSTRAP-nif-planning.md` — the two planning
  frames; scope discipline and the "one message representation across realtime
  and files" goal that the audit's vocabulary findings bear on.

## 2. The seed set CC was given (and its provenance)

CC was handed a **calibration set** of findings (in `AUDIT-PROMPT-cc.md §5`)
found during planning analysis and verified by reading. Two consequences for you:

- **Those seed findings originated with the analyst, not with CC.** Scrutinize
  CC's confirmation of them as hard as its novel findings — and verify the
  analyst's characterizations too. If a seed finding is wrong or mis-severity,
  that is a real finding about the scoping, not a given.
- **The audit's value is what CC adds beyond the seed.** An audit that only
  re-states §5 has failed the independence test. Explicitly check whether CC
  went past the floor.

The seed headline items, so you know what was "claimed known" going in (verify
each at the cited line):

- Three incompatible message vocabularies across `midimsg` / `midibin` /
  `midifile` despite the "lingua franca" claim (`midimsg.erl:11-16`, `README.md:31`).
- SysEx single-7-bit-byte only: `midibin.erl:137-138` (decode), `:410-411`
  (encode); `midimsg:sys_ex/1` takes a `binary()` the codec can't encode.
- `midifile` `seq_name` writes `?META_TRACK_END` instead of `?META_SEQ_NAME`
  (`midifile.erl:319-321`).
- `?META_SEQUENCER_SPECIFIC` reads as `{seq_name,…}` (`midifile.erl:188`).
- `track_end` read/write tuple-arity mismatch (`:147` vs `:309`).
- `write/2` hardcodes format 1 (`:247`) while `read/1` accepts any format (`:51-53`).
- Running-status reconstruction assumes 3-byte events (`:201-207`).
- `var_len/1` overflow does `exit("…" ++ I)` with integer `I` → `badarg` (`:381-382`).
- Process-dictionary state for running status (`:73-74`, `:202-203`, `:359-361`).
- String error reasons (`include/errors.hrl:1-3`); empty `midierrs.erl`; sparse `-spec`.
- Tests cover `midibin` only; `sys_ex_test` bakes in the single-byte limit
  (`test/midibin_tests.erl:224-227`); batch-shape mismatch vs `midimsg:batch`.

## 3. How to evaluate CC's audit

**Per finding — assign a written disposition** (closure discipline):

- **Confirmed** — you opened the cited `file:line` (and PDF, for spec claims) and
  the failure mode is real as described.
- **Confirmed, severity adjusted** — real, but CC's Blocker/High/Medium/Low is
  wrong; state the correct level and why.
- **Misdiagnosed** — the symptom is real but CC's root cause or fix is wrong.
- **Not a bug** — CC erred; show why at the line.
- **Needs repro** — plausible but unverified; name the experiment (often a
  `rebar3 shell` round-trip or a `pdftotext` grep) that would settle it.

**Evidence access.** Open every cited line yourself. For spec-adherence findings,
open the cited PDF section — a confident-sounding spec paraphrase that the PDF
does not support is itself a finding (downgrade and flag it).

**Severity discipline.** Check CC didn't use Medium as a hedge, and that every
"Blocker" really ships a bug to a user (e.g. corrupt SMF output, a crash on a
reachable library path). A SysEx-can't-encode or a `seq_name`-writes-track-end is
plausibly Blocker; a missing `-spec` is not.

**Miss / silent-drop detection.** Diff CC's findings against (a) the §2 seed set
and (b) the consumer needs in `NEEDS-from-midiio.md`. Anything in either that CC
did **not** surface is a miss — list it with `file:line`. Specifically confirm CC
caught: the SysEx single-byte limit; the three-vocabulary problem; the
`seq_name`→`track_end` write bug; format-1-only write; the `track_end` arity
mismatch; process-dictionary state; string error reasons; and the
`midifile`/`midimsg`/`midiutil` **testing gap**.

**Overclaim / generic detection.** Flag any CC finding that is generic advice
with no `file:line`, or that restates the code instead of naming a failure mode,
or that asserts a behavior without having run it where running is cheap.

**Trending.** Cluster root causes. The analyst suspects two systemic clusters —
the **message-vocabulary split** and `midifile`'s **process-dictionary state
machine** — plus a **MIDI-1.0-only scope** that makes some "missing feature"
findings non-bugs. Confirm, refute, or add clusters; a systemic root cause is
worth more to the fix round than its scattered instances.

## 4. Structural pulls to watch — in CC and in yourself

- **Corpus pull (on CC):** `midifile` is the venerable ~2010 Jim Menard SMF
  reader; its canonical status invites under-criticism. Venerability is not
  correctness — it has real bugs (see seed set). Check CC didn't wave it through.
- **Objective pull (on CC):** spec claims that *sound* right but don't match the
  PDF; confident severities with no evidence. These are exactly what your
  evidence-access pass is for.
- **Alignment pull (on you):** the two failure modes are rubber-stamping CC to
  be agreeable, and inflating dispositions to *look* rigorous. Calibrate to the
  evidence, not to either appearance.
- **Capability (on you):** when a disposition turns on runtime behavior — does
  writing a round-tripped `track_end` actually crash? does a 300-byte SysEx
  really fail to encode? — **run it** in a sandboxed `rebar3 shell` rather than
  reasoning about it. Faking verification is worse than naming the uncertainty.

## 5. Output

Write `workbench/audit/<DATE>-audit-eval-cdc.md` (run `date +%Y.%m.%d`). Contents:

1. **Verdict** — 3–5 sentences: is the audit fit to drive a fix round? What is
   its dominant weakness, if any?
2. **Disposition table** — one row per CC finding: ID/location, CC severity, your
   disposition (§3), one-line basis.
3. **Misses** — seed-set and consumer-need items CC did not surface, with
   `file:line`.
4. **Overclaims / unsupported** — CC findings that fail evidence or severity
   discipline.
5. **Trends** — the systemic root-cause clusters, confirmed or revised.
6. **Spec-citation audit** — for each spec-adherence finding, whether the cited
   PDF section actually supports it.
7. **Closure** — counts by disposition, and the go/no-go for the fix round.

## 6. Definition of done

Every CC finding has a written disposition; misses vs. the seed set and the
consumer needs are enumerated with `file:line`; spec claims are checked against
the actual PDFs; systemic trends are named; and there is a clear, evidence-backed
go/no-go on whether CC's audit is solid enough to plan the midilib fix round
from. Unresolved items are named and tracked — no silent drops, no spec-softening.
