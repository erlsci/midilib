# CC follow-up — Arc 4 · Slice A: skip alien chunks (close F-CDC-1)

> A small, surgical follow-up on the *already-closed* slice-A reader. One CDC
> finding to fix, with tests and a ledger row. Not a re-open of the slice — the
> 20 original rows stand; this adds row 21 and resolves the finding.

## The finding (F-CDC-1, from `cdc-verification.md`)

`midifile:parse_tracks/3` crashes (`function_clause`) on a chunk whose type is not
`MTrk` while tracks remain to be read. The closing report justified this as
"let-it-crash (R4)." **That is the wrong call here**, and it's worth being precise
about why, because the distinction matters elsewhere too:

- **"Let it crash" is for the *unexpected* — programmer errors and genuinely
  malformed input** (a truncated event, a data byte with no running status, a
  declared length that overruns the buffer). For those, crashing is correct: the
  supervisor restarts from a known-good state and the bug isn't masked.
- **An alien chunk is not unexpected — the SMF spec explicitly anticipates it.**
  RP-001 (Standard MIDI Files), Conventions: a program *"should expect alien
  chunks and treat them as if they weren't there"* — i.e. read the 4-byte type and
  4-byte length, skip that many bytes, and carry on. A chunk type you don't model
  is a *normal, spec-legal input*, so handling it is ordinary control flow (EH:
  expected outcomes are values / normal handling; exceptions are exceptional).

Crashing on a spec-legal file is a robustness regression, too: the old
`look_for_chunk` scanned for the `MTrk` cookie and so skipped alien chunks; the
rewrite is stricter than its predecessor and than the spec requires.

## The fix

Add a chunk-skip clause to `parse_tracks/3`. Every SMF chunk — known or alien —
has the shape `<4-byte type><32-bit big-endian length><length bytes>`, so an alien
chunk is skippable by its own declared length. `ntrks` counts **`MTrk` chunks
only** (RP-001), so an alien chunk is skipped *without* decrementing the track
count.

Sketch (re-derive, don't paste blindly):

```erlang
parse_tracks(_Bin, 0, Acc) ->
    {ok, lists:reverse(Acc)};                       % all tracks read; trailing bytes/chunks ignored
parse_tracks(<<"MTrk", Len:32, TrackData:Len/binary, Rest/binary>>, N, Acc)
  when N > 0 ->
    case parse_track(TrackData) of
        {ok, Track}        -> parse_tracks(Rest, N - 1, [Track | Acc]);
        {error, _} = Error -> Error
    end;
%% Alien chunk (RP-001: "expect alien chunks and treat them as if they weren't
%% there"). Skip its declared length; do NOT count it as a track.
parse_tracks(<<_Type:4/binary, Len:32, _Skip:Len/binary, Rest/binary>>, N, Acc)
  when N > 0 ->
    parse_tracks(Rest, N, Acc).
```

Clause-ordering note (FP-12): the `MTrk` clause must precede the alien clause —
both would match an `MTrk` chunk, and `MTrk` must win. Confirm no shadowing.

### What stays a crash (do NOT soften)

These remain let-it-crash — they are genuinely malformed, not spec-anticipated:
- a chunk whose declared length overruns the remaining bytes (a truncated /
  corrupt chunk — neither clause matches);
- the header declaring more `MTrk` chunks than the file actually contains (bytes
  run out with `N > 0`);
- the malformed-track-body cases already covered (truncated event, dangling data
  byte, zero division).

The point is not "stop crashing"; it's "crash on the malformed, *handle* the
spec-legal." Don't over-correct into swallowing real corruption.

## Acceptance (new ledger row 21)

Add to `ledger.md` and close with evidence (CC), then CDC re-verifies:

| # | Acceptance criterion | Evidence to cite |
|---|----------------------|------------------|
| 21 | An alien (non-`MTrk`) chunk is skipped per RP-001, not crashed on: one before the first track, one between tracks, and one trailing after the last track all read correctly; a *truncated* chunk (declared length overruns) still crashes | `alien_chunk_before_first_track_test`, `alien_chunk_between_tracks_test`, `alien_chunk_trailing_ignored_test`, `truncated_chunk_still_crashes_test` (e.g. `?assertError(_, …)`) |

Fixtures are inline binaries, as in the rest of `midifile_tests.erl` (e.g. an
alien chunk `<<"XYZW", 3:32, 1, 2, 3>>`). Keep the "visible bytes" discipline.

## Constraints

- snake_case; `-spec`s unchanged (`parse_tracks/3`'s spec already fits); no
  process dictionary; portable (OTP 22–29, no `-doc`).
- Touch only `src/midifile.erl` (the one clause) and `test/midifile_tests.erl`
  (the new fixtures). Do **not** touch `midi_codec`, `midibin`, `midi_msg.hrl`, or
  `midierrs`. Do not re-open the other 20 rows.
- N-CDC-1 (uniform encode-side error stance) is **out of scope** — it is an
  encode/writer concern and belongs to slice B.

## Done

Row 21 closed with cited evidence; `rebar3 as test check` green and `rebar3 proper`
green; the existing 107 eunit tests + 2 properties still pass (plus the 4 new
fixtures). Append a short "Follow-up 1" section to `closing-report.md` (what
changed and why) rather than rewriting it. CDC then updates `cdc-verification.md`
to mark F-CDC-1 **closed**. Five-iteration cap; surface any blocker rather than
dropping it.
