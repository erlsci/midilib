# midilib v0.6.0 — release plan

**Status:** planning (arc breakdown)
**Current version:** 0.5.2 → **target 0.6.0**
**Stability:** pre-1.0; **breaking changes permitted and expected**.
**Drives from:** `arc00-audit/2026.06.17-audit-index.md` (CC audit) and
`arc00-audit/2026.06.17-audit-eval-cdc.md` (CDC evaluation — verdict: GO).

## 1. Why this release exists

The audit (4 reports, ~20 distinct defects, 4 strict Blockers) and its
independent evaluation found that midilib's defects are not scattered — they
cluster around three systemic roots:

- **A. The message-vocabulary split.** `midimsg`, `midifile`, and `midibin`
  speak three different message shapes *and* three different tag-name sets for
  the same concepts (`note_on`/`note_off` vs `on`/`off`; `time_sig` vs
  `time_signature`; `tempo_bpm` vs `tempo`; …), under a "lingua franca" claim
  that is false. (CDC finding M1 — deeper than the original audit recorded.)
- **B. `midifile`'s process-dictionary state machine.** Running-status state
  threaded through `put`/`get` across ~20 clauses — non-reentrant, untestable,
  and the reason read/write drifted apart unnoticed.
- **C. Inherited, untested `midifile`.** All four strict Blockers live in the
  2010-vintage `midifile`; the modern, tested `midibin` is clean.

**The keystone decision:** a single canonical message/event vocabulary that all
three modules adopt. Once it exists, a whole cluster of findings collapses into
it — the three-vocabulary split (#12/C2), the batch-shape mismatch (#11/C3),
the channel-base divergence (#13/C7), the forked error contract (#14/C4), and
normalization ownership (C8). That is why this release is **vocabulary-first**:
design the canonical vocabulary, then move each module onto it; the Blockers are
fixed *as part of* the rewrite, not as a separate patch round.

This also fixes the family-level blocker: `midi`/`midiio` are about to route
every message through midilib's codec, and today multi-byte SysEx is uncodable
(R7), the vocabulary isn't unified (R6/R1), and the error contract gives `midi`
nothing uniform to branch on (R4).

## 2. Scope

### In scope
- One canonical message/event vocabulary (records and/or tagged tuples), adopted
  by `midimsg`, `midibin`, `midifile`.
- All 4 strict Blockers + the High/Medium findings that ride the three clusters.
- A real test harness: `midifile`/`midimsg`/`midiutil` coverage, a round-trip
  PropEr property, a non-zero coverage floor.
- The architecture/design document the repo has never had (audit #25 / index note).
- Build/dev tooling (this release's arc 1) and docs/spec/`-spec` completeness.

### Explicitly NOT in scope (disclosed, not dropped)
- **MIDI 2.0 / UMP** — midilib stays a MIDI 1.0 library; the 2.0 PDFs are
  reference only. No 1.0/2.0 conflation existed and none is introduced.
- **The "make `midifile` distributed" TODO** (`midifile.erl:57`) — design the
  state-threaded rewrite so it *could* be parallelised, but don't build it.
- **Deep performance work** beyond the audit's named items (#21 batch O(n²),
  #22 chunk scan).
- **Actually shipping `midiio:send_batch/2`** — that's a midiio concern (R9);
  midilib only guarantees its `encode_batch/2` output shape.

## 3. Arc breakdown

Sequencing is **vocabulary-first, single clean release**. Dependencies are
load-bearing: an arc may not start until the arc(s) it depends on close.

| Arc | Name | Depends on | Closes (audit/CDC findings) |
|----:|------|------------|------------------------------|
| 1 | **Build system & dev workflow** | — | M4 (coverage gate); sets the harness all later arcs use |
| 2 | **Canonical vocabulary + error contract + design doc** | 1 | keystone for #12/C2; #16, #24 (error vocab home); architecture-doc gap (#25/index); records the channel-base + normalization-ownership invariants |
| 3 | **`midibin` onto the vocabulary + SysEx codec** | 2 | #8/S1/C1 (Blocker), #11/C3, #13/C7 (decision), #14/C4, C5, #21, #20 |
| 4 | **`midifile` rewrite (state-threaded, round-trip-tested)** | 2, 3 | #1/S2, #2/S3, #3 (Blockers), #4/S4, #5/#17, #6/S5, #7/S8/C8, S7, #15, #18 (cluster B), #19/#20, #22, #26, M2 |
| 5 | **`midimsg` + `midiutil` alignment** | 2, 3 | #9/S6/C6, #10, #23, M3; orphaned/deprecated constructors + LFE aliases |
| 6 | **Docs, specs, release polish** | 3, 4, 5 | #25 (`-spec`/`-doc`/`-moduledoc`), README rewrite, CHANGELOG, version bump → 0.6.0, hex prep |

Notes:
- **Arc 2 is the design heavy-lift** — its design slice *is* the "truly unified
  interchangeable MIDI abstractions" brainstorm. It produces the design doc
  before any module is moved.
- **Arc 3 before Arc 4** deliberately: `midibin` is small, clean, and tested, so
  it's the cheapest place to prove the canonical type before the larger
  `midifile` rewrite leans on it.
- **Arc 4 is an arc, not a slice** — it will split into slices (e.g. reader,
  writer, state-threading, round-trip property) at its own arc-plan time.

## 4. Definition of done (release)

- Every strict Blocker (#1, #2, #3, #8) fixed, each with a test that fails on
  the old code and passes on the new.
- `read(write(Seq)) =:= Seq` holds as a PropEr property over generated sequences.
- One canonical vocabulary; the README "lingua franca" claim is *true* and
  documented (channel base + normalization ownership written down).
- `midibin`, `midifile`, `midimsg`, `midiutil` all have tests; coverage floor
  raised from 0 to a real threshold and enforced by `make check` / CI.
- Architecture/design doc exists; every exported function has a precise `-spec`
  and `-doc`.
- CHANGELOG enumerates the breaking changes; consumer impact noted for
  `midi`/`midiio` (R1/R4/R6/R7/R9).
- No silent drops: every audit finding is closed in an arc or explicitly
  deferred with rationale here.

## 5. Findings explicitly deferred (with rationale)
*(none yet — populated if any finding is consciously pushed past 0.6.0)*
