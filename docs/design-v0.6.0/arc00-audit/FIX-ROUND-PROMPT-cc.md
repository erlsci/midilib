# Fix-round prompt — applying the midilib audit (CC, ledger-driven)

> Paste this as the opening message of a fresh session. It is self-contained;
> assume you remember nothing of prior conversations.
>
> **Precondition:** this round does not start until both exist in
> `workbench/audit/`: CC's audit reports (`<DATE>-audit-results-*.md`) **and**
> the twin's disposition report (`<DATE>-audit-eval-cdc.md`). If either is
> missing, stop — the inputs aren't ready.
>
> This is **implementation**, not diagnosis. It runs under ledger discipline:
> CC implements, CDC verifies independently, five-iteration cap per slice.

## 0. How to start

1. Load the **collaboration-framework** skill; read `AI-CONSTITUTION-SUPPLEMENT.md`,
   `AI-ENGINEERING-METHODOLOGY.md`, and `templates/LEDGER_DISCIPLINE.md`. The
   last one is the contract for this round — read it before writing any code.
2. Load the **erlang-guidelines** skill (`11-anti-patterns.md` first; then the
   chapters touching what you change — likely `03-error-handling`,
   `04-data-and-types`, `05-functions-and-pattern-matching`, `13-documentation`,
   `15-testing`, `17-tooling`).
3. Read the inputs in §2 before scoping anything.
4. Peer frame, boldness default, write to the floor. The ledger's per-row walk
   is the antidote to compliance theatre — trust it over the instinct to report
   "deviations: none."

## 1. What this round is

The audit (diagnosis) and the twin's independent evaluation (dispositions) are
done. This round **applies the fixes the twin confirmed**, one ledger row per
fix, each backed by a regression test and reproducible evidence. It is
deliberately *not* a redesign: anything that changes midilib's public message
representation or the `seq` shape is routed to a separate design arc (§3), not
forced through a bug-fix ledger.

## 2. Inputs (read in order)

1. **The twin's disposition report** — `workbench/audit/<DATE>-audit-eval-cdc.md`.
   **This gates scope.** Each audit finding has a disposition: Confirmed,
   Confirmed-severity-adjusted, Misdiagnosed-corrected, Not-a-bug, or Needs-repro.
2. **The audit reports** — `workbench/audit/<DATE>-audit-results-erlang.md`,
   `-midi-spec.md`, `-consumer-fit.md`. The findings themselves (location,
   failure mode, proposed fix).
3. **The audit assignment** — `workbench/audit/AUDIT-PROMPT-cc.md` (the four
   dimensions and the seed set, for context on intent).
4. **Consumer context** — `../../../midi/workbench/NEEDS-from-midiio.md` (esp.
   **R6** normalization ownership and **R7** SysEx) and `UPSTREAM-minimidio.md`.
   These tell you which behaviors are *intentional* (§4, do-not-fix) and which
   gaps are real.
5. **The code** — `src/*.erl`, `include/midi.hrl`, `include/errors.hrl`,
   `test/midibin_tests.erl`, `rebar.config`.

## 3. Scope gate — what is in this round, what is routed to design

**In scope (fix here):** a finding qualifies only if the twin marked it
**Confirmed / Confirmed-severity-adjusted / Misdiagnosed-corrected**, *and* the
fix is **local** — a correctness or hygiene change that does not alter midilib's
public message vocabulary or the `seq`/event tuple shapes that downstream code
pattern-matches.

**Routed to a separate design arc (do NOT fix here — defer with rationale):**
these are real and important, but each requires a representation decision that
the consumer (`midi`) must be party to. Record each in the ledger as `deferred`
with a re-entry condition pointing at the design session; do not silently drop
them and do not quietly redesign under a bug-fix row.

- **Message-vocabulary unification.** Reconciling the three shapes across
  `midimsg` / `midibin` / `midifile` changes the entire public surface and
  directly serves `midi`'s "one representation across realtime and files" goal —
  it is a v-next API design, not a fix. **Top design priority.**
- **Multi-byte SysEx codec (R7).** Borderline: if the twin and the design owner
  have already settled the term representation (e.g. `{midi, {sys_ex, Bytes}}`
  with `Bytes` the payload binary), a *local, tested* encode/decode fix MAY be
  pulled in scope. If it implies structured manufacturer-ID / fragmented-SysEx
  handling, defer to design. Default: defer, flagged as the #1 cross-layer
  blocker.
- **SMF format 0/2 fidelity.** Faithful round-trip needs the `seq` tuple to
  carry the original format word, which ripples to `midiutil` and `undermidi`.
  Design.

**Excluded:** anything the twin marked **Not-a-bug**. Anything **Needs-repro**
must be reproduced (or dropped) *before* it can earn a ledger row — a fix without
a confirmed defect is speculative.

**If a fix grows:** if a row you believed local turns out to require a
public-shape change, **stop and raise an amendment** (reclassify it to design).
Do not expand a bug-fix into a redesign inside the ledger. This is the single
most important guardrail in this round.

## 4. Do NOT "fix" these — they are by design

Guard against over-fixing. Confirm against the twin's Not-a-bug dispositions, but
expect at least these:

- **`midibin` does not fold note-on-velocity-0 to note-off.** This is correct:
  per `midi`'s **R6**, the codec is byte-faithful and *normalization lives in
  `midi`*, not midilib. Do not add folding to `midibin`. (`midifile` folding
  vel-0 on SMF read, `midifile.erl:100-105`, is the separate SMF convention and
  is acceptable — leave it unless the twin flagged it.)
- **The `{midi, ...}` wrapper** on every term is intentional (tuple-collision
  avoidance in multi-purpose gen_servers, per `README.md:35`). Don't strip it.
- **MIDI 2.0 / UMP absence.** midilib is a MIDI 1.0 library; missing 2.0 is scope,
  not a bug.

## 5. Build the ledger

Construct a ledger from the in-scope findings (§3). One row per finding, using
the `LEDGER_DISCIPLINE.md` columns:

| ID | Criterion | Verify | Significance | Origin | Status | Evidence | Notes |

- **Criterion** — a single observable claim ("`midifile` writes `seq_name` as
  meta type `0x03`, not `0x2F`").
- **Verify** — an *executable* check CDC can reproduce. For every correctness
  fix this **must be a regression test that fails before the fix and passes
  after** — not a test that merely compiles. The audit found `midifile`,
  `midimsg`, and `midiutil` have **zero tests**, so fixing a `midifile` bug means
  authoring the first `midifile_tests`. A grep may supplement but does not
  replace the test for a behavioral fix.
- **Significance** — serious / correctness-grade / polish (map from the twin's
  severity).
- **Origin** — the audit finding ID + the twin's disposition.
- **Status** starts `open`; reaches `done` (commit SHA + Verify output),
  `deferred` (reason + re-entry condition), or `no-op` (rationale). `open` is not
  final; missing rows are ledger bugs.

**Slicing.** Size each slice to one context with headroom (methodology). A
sensible default, which you may revise:

- **Slice 1 — `midifile` correctness** (the defect-dense module): e.g.
  `seq_name`→`?META_SEQ_NAME` (`midifile.erl:319-321`), `?META_SEQUENCER_SPECIFIC`
  tag (`:188`), `track_end` read/write arity (`:147`/`:309`), `var_len/1`
  overflow `exit` on integer (`:381-382`), running-status framing for 2-byte
  messages (`:201-207`) — **plus the first `midifile_tests` round-tripping each.**
- **Slice 2 — hygiene & contracts:** string→atom error reasons with context
  (`include/errors.hrl:1-3`), remove dead `src/midierrs.erl`, `read/1` error
  contract → tagged `{ok,_}|{error,_}` (`midifile.erl:37-38`), add `-spec` /
  `-moduledoc` to touched functions, resolve `rt_tick` (no encoder —
  `midimsg.erl:296-298`; decide alias-to-clock vs remove, with the twin).

One slice = one ledger. Put each slice's artifacts under
`workbench/fix-<DATE>/sliceNN-<slug>/` (`ledger.md`, `cc-closing-report.md`;
CDC adds `cdc-verification.md`). Open slice 2's ledger only after slice 1 closes.

## 6. CC protocol (from LEDGER_DISCIPLINE)

1. Read the ledger before writing code; if a criterion is unclear, ask before
   implementing.
2. Work against the ledger, not around it — discovered problems become amendment
   requests, not silent workarounds (see §3 "if a fix grows").
3. Fill Evidence at the commit where each criterion is met; don't defer all
   evidence to the end.
4. In the closing report, **walk the ledger row by row** — a disposition and
   evidence for every numbered row. No prose summary; no "deviations: none."
5. Name uncertainty: "done with caveat X" beats a confident "done" that's
   softpedalled.
6. Keep the whole suite green: `rebar3 as test check` (compile, xref, dialyzer,
   eunit, coverage) must pass; touched functions get `-spec` so dialyzer stays
   clean. Do not hide a failure behind a skipped test. Do not bump the version or
   publish.

**Iteration budget: five per slice.** 1–3 is normal; 4–5 means the slice was
too big (tighten the next one); at 5 without convergence, stop and rework scope
or restart with fresh context rather than grind a sixth pass.

## 7. Handoff to CDC

When a slice's ledger is closed, the twin verifies it per the **CDC protocol** in
`LEDGER_DISCIPLINE.md`: count rows against the opening ledger (missing rows are
bugs), run *every* `done` row's Verify command (proposed-done until reproduced),
check `deferred` reasons and re-entry conditions, check `no-op` rationales, and
grep workspace-wide for partial adoption and spec-softening. CDC's rejection of a
softpedalled row counts as a new iteration against the five-cap. The twin already
holds the family frame from `SESSION-BOOTSTRAP-audit-eval-cdc.md`.

## 8. Definition of done (this round)

Every in-scope finding has a ledger row at a final status with reproducible
evidence; every behavioral fix has a regression test that fails without it;
`rebar3 as test check` is green; the three design-routed items (§3) are recorded
as `deferred` with re-entry conditions pointing at the design session; each slice
has a per-row closing report; and CDC has verified each closed ledger. Unresolved
items are named and tracked — no silent drops, no spec-softening, no bug-fix that
grew into an unannounced redesign.
