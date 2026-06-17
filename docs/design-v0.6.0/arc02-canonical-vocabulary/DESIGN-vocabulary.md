# Arc 2 — Canonical MIDI message vocabulary (design)

**Release:** v0.6.0 · **Arc:** 2 of 6 · **Status:** design accepted, implementing
**Closes / enables:** the root cause of cluster A (audit #12/C2, CDC M1); homes
the error vocabulary (#16, #24); is the prerequisite for arcs 3–5.
**Inputs:** the CC audit, the CDC evaluation, `midi/workbench/NEEDS-from-midiio.md`
(R1/R4/R6), `midi/workbench/SESSION-BOOTSTRAP-api-planning.md` (§4, §6, the
"one representation across realtime and files" goal).

---

## 1. The problem

Three modules speak three vocabularies — different *shapes* and different *tag
names* for the same concepts:

| concept | `midibin` (wire) | `midifile` (SMF) | `midimsg` (ctor) |
|---|---|---|---|
| note on | `{midi,{note_on,[{channel,N},{pitch,P},{velocity,V}]}}` | `{on, DT, [Chan,Note,Vel]}` | `{midi,{note_on,[…]}}` |
| track name | — | `{seq_name, DT, "…"}` | `{midi,{track_sequence_name,"…"}}` |
| tempo | — | `{tempo, DT, [Usec]}` | `{midi,{tempo_bpm, Bpm}}` |
| channel base | 1-based | 0-based | n/a |

The result: `midi` cannot stream one representation through both the live path
and the file path; `midibin` can encode only a subset of what `midimsg`
produces; round-trips drift. This arc defines **one** vocabulary all three
adopt.

## 2. The keystone: separate *what* from *when*

The single clarifying move is to stop baking timing into message identity.

- **`message()`** — the timing-free vocabulary. Channel-voice, channel-mode,
  system-common, system-real-time, system-exclusive, and (file-only) meta
  events. This is the representation that "flows through live device I/O and
  through `midifile:read/1`."
- **`event()`** — a `message()` situated in an SMF timeline: a delta-time plus a
  `message()`. A track is a list of events; a live message is a bare
  `message()` (its timing is its moment of arrival).

```
live:  midiio --bytes--> midibin:decode/1 --> message()
file:  midifile:read/1 --> [#event{delta, message()}]   (message() is the same type)
build: midimsg:note_on(...) --> message()
```

One vocabulary, two contexts. This delivers the api-planning "one
representation across realtime and files" structurally, not by convention.

## 3. Representation decisions (accepted)

1. **Per-type records**, defined in a shared header `include/midi_msg.hrl`.
   Rationale: Dialyzer-checkable (replaces the useless `-> tuple()` specs,
   audit #25), a single-place schema the whole family reads, named fields
   (kills the positional `[Chan,Note,Vel]` lists), ergonomic pattern matching.
2. **No `{midi, _}` envelope.** Records self-tag, so collision-safety (the
   README's stated reason for the envelope) is already met; API-boundary
   disambiguation is carried by the `{ok, _} | {error, _}` return convention.
   The `{midi, _}` wrapper is a *process-routing* concern that belongs to the
   consumer (`midi`), not to a codec library — so midilib returns bare records
   and lets `midi` wrap for its own mailbox discipline.
3. **Channel base is 1-based (1..16)** canonically — matches `midibin` today,
   human numbering, and the spec's "Channel #" (Detailed Spec Table I). The raw
   0..15 nibble exists only inside the wire codec (`midibin`) and file codec
   (`midifile`); conversion happens at those edges and nowhere else.
4. **Full per-type meta records** — maximum round-trip fidelity and Dialyzer
   coverage; unknown metas are preserved verbatim via `#meta_unknown{}`.
5. **Normalization stays out of the codec** (consumer C8/R6): a Note-On with
   velocity 0 decodes to `#note_on{velocity = 0}`, never folded to a note-off.
   The fold is `midi`'s job; the vocabulary keeps the two distinct so `midi`
   can apply the canonical rule with a single source of truth.

## 4. The record catalogue

Field value types: `channel() :: 1..16`, `data7() :: 0..127`,
`value14bit() :: 0..16383`.

### Channel-voice (`channel_message()`)
```erlang
-record(note_off,           {channel, pitch, velocity}).
-record(note_on,            {channel, pitch, velocity}).   % velocity 0 NOT folded
-record(poly_aftertouch,    {channel, pitch, pressure}).
-record(control_change,     {channel, control, value}).
-record(program_change,     {channel, program}).
-record(channel_aftertouch, {channel, pressure}).
-record(pitch_bend,         {channel, value}).             % value14bit(), centre 8192
```

### Channel-mode (`channel_message()`)
Modelled distinctly (not as raw CC 120–127) to preserve their semantics, with
the spec's mode names:
```erlang
-record(channel_mode, {channel, mode, value = 0}).
%% mode :: all_sound_off | reset_all_controllers | local_control_off
%%       | local_control_on | all_notes_off | omni_mode_off | omni_mode_on
%%       | mono_mode_on | poly_mode_on
%% value: mono_mode_on carries the channel count; 0 otherwise.
```

### System-common + real-time + exclusive (`system_message()`)
```erlang
-record(mtc_quarter_frame, {message_type, value}).  % type 0..7, value 0..15
-record(song_position,     {position}).             % value14bit()
-record(song_select,       {song}).
-record(tune_request,      {}).
-record(end_of_exclusive,  {}).                     % lone F7
-record(realtime,          {type}).
%% type :: clock | start | continue | stop | active_sensing | reset
-record(sysex, {data :: binary()}).
%% data is the payload BETWEEN F0 and F7 (manufacturer id + body); the codec
%% adds the F0/F7 framing on encode and strips it on decode. Arbitrary length —
%% fixes the single-byte limit (audit #8/S1/C1).
```

### Meta (`meta_message()`, file-only)
```erlang
-record(meta_sequence_number,    {value}).
-record(meta_text,               {text}).
-record(meta_copyright,          {text}).
-record(meta_track_name,         {name}).
-record(meta_instrument_name,    {name}).
-record(meta_lyric,              {text}).
-record(meta_marker,             {text}).
-record(meta_cue_point,          {text}).
-record(meta_channel_prefix,     {channel}).
-record(meta_set_tempo,          {usec_per_quarter}).
-record(meta_smpte_offset,       {hour, minute, second, frame, sub_frame}).
-record(meta_time_signature,     {numerator, denominator,
                                  clocks_per_click, notated_32nd_per_quarter}).
-record(meta_key_signature,      {key, mode}).      % key -7..7, mode major|minor
-record(meta_sequencer_specific, {data :: binary()}).
-record(meta_end_of_track,       {}).
-record(meta_unknown,            {type, data :: binary()}).  % round-trip preserve
```

Notes:
- `meta_time_signature.denominator` is the **human** denominator (e.g. `8` for
  6/8); the codec converts to/from the spec's `2^dd` exponent. Only powers of 2
  are representable (a spec constraint, not ours).
- A non-spec meta (e.g. MIDI Port `FF 21`) round-trips through `#meta_unknown{}`
  — no information lost.

### Container types
```erlang
-record(event, {delta :: non_neg_integer(), message :: message()}).
-record(track, {events :: [#event{}]}).
-record(seq,   {format   :: 0..2,
                division :: {ppqn, pos_integer()}
                          | {smpte, pos_integer(), pos_integer()},
                tracks   :: [#track{}]}).
```

### Union types (the contract surface)
```erlang
-type channel_message() :: #note_off{} | #note_on{} | #poly_aftertouch{}
                         | #control_change{} | #program_change{}
                         | #channel_aftertouch{} | #pitch_bend{}
                         | #channel_mode{}.
-type system_message()  :: #mtc_quarter_frame{} | #song_position{}
                         | #song_select{} | #tune_request{}
                         | #end_of_exclusive{} | #realtime{} | #sysex{}.
-type meta_message()    :: #meta_sequence_number{} | #meta_text{}
                         | #meta_copyright{} | #meta_track_name{}
                         | #meta_instrument_name{} | #meta_lyric{}
                         | #meta_marker{} | #meta_cue_point{}
                         | #meta_channel_prefix{} | #meta_set_tempo{}
                         | #meta_smpte_offset{} | #meta_time_signature{}
                         | #meta_key_signature{} | #meta_sequencer_specific{}
                         | #meta_end_of_track{} | #meta_unknown{}.
-type message()         :: channel_message() | system_message() | meta_message().
-type event()           :: #event{}.
```

## 5. Tag-name migration (the M1 fix)

| concept | old (`midifile`) | old (`midimsg`) | canonical |
|---|---|---|---|
| note on/off | `on` / `off` | `note_on` / `note_off` | `note_on` / `note_off` |
| poly pressure | `poly_press` | `poly_aftertouch` | `poly_aftertouch` |
| control change | `controller` | `cc` | `control_change` |
| channel pressure | `chan_press` | `aftertouch` | `channel_aftertouch` |
| pitch bend | `pitch_bend` | `pitch_bend` | `pitch_bend` |
| track name | `seq_name` | `track_sequence_name` | `meta_track_name` |
| set tempo | `tempo` | `tempo_bpm` | `meta_set_tempo` (µs/qn) |
| time signature | `time_signature` | `time_sig` | `meta_time_signature` |
| key signature | `key_signature` | `keysig` | `meta_key_signature` |
| sequence number | `seq_num` | `sequence_number` | `meta_sequence_number` |
| sequencer-specific | `seq_name`(!) | `sequencer_data` | `meta_sequencer_specific` |

(`tempo_bpm` becomes a `midimsg` *convenience constructor* that converts BPM →
µs/qn and returns `#meta_set_tempo{}`; BPM is not a separate wire concept.)

## 6. Error contract (homes audit #16, #24)

`midierrs.erl` becomes the error vocabulary + `format_error/1` (giving the
dead module a real job). Reasons are **atoms/structured terms**, never strings:

```erlang
-type reason() :: non_midi
                | not_implemented
                | {unsupported, term()}
                | {unknown, binary()}          % decode: well-formed but unhandled
                | {bad_vlq, binary()}          % read: malformed variable-length qty
                | {bad_value, atom(), term()}  % encode: field out of range
                | {open, file:name_all(), term()}
                | {not_midi_file, file:name_all()}.
```

Return conventions all modules adopt (in arcs 3–5):
- `midibin:decode/1 -> {ok, message()} | {error, {unknown, binary()}}`
- `midibin:encode/1 -> {ok, binary()} | {error, reason()}`
- `midifile:read/1  -> {ok, #seq{}} | {error, reason()}`
- `midifile:write/2 -> ok | {error, reason()}`

`format_error/1` maps every reason to a human string (the strings move *out* of
the matchable channel, fixing audit #16).

## 7. Scope boundary for this arc

This arc delivers the **vocabulary and the error module only** — it does *not*
rewire the codecs:

- `include/midi_msg.hrl` (records + types) and `midierrs.erl` (reasons +
  `format_error/1`) are added now. The header is inert until modules include it.
- `midibin` adopts records + the `{ok,_}|{error,_}` contract in **Arc 3**;
  `midifile` in **Arc 4**; `midimsg`/`midiutil` in **Arc 5**. The legacy
  `include/errors.hrl` string macros stay until Arc 3 migrates `midibin` and its
  tests, so the build/tests do not break mid-arc.
- The wire/file encoding constants (status nibbles, meta type bytes) remain in
  `include/midi.hrl` — they are codec-internal, distinct from the public
  vocabulary in `midi_msg.hrl`.

## 8. Alternatives considered

- **Typed tagged tuples** (`{note_on, Ch, P, V}` + `-type` unions): Dialyzer-OK
  and no header to share, but positional — the readability problem the audit
  flagged. Rejected in favour of named record fields.
- **Tagged maps** (`#{type => note_on, …}`): extensible and serialization-
  friendly, but no compile-time field checking and slower matching. Rejected for
  a fixed, well-known domain where the schema *should* be closed and checked.
- **Keep `{midi, _}`**: rejected — see §3.2.
- **Fold note-on-vel-0 in the codec**: rejected — violates consumer C8/R6.

## 9. Consumer coordination (disclosed changes for `midi`/`midiio`)

- **`#seq{}` drops the explicit conductor-track split.** Today's
  `{seq, Header, ConductorTrack, Tracks}` (referenced in api-planning §4)
  becomes `#seq{format, division, tracks}` with a uniform track list — the
  format-1 "conductor" is simply `hd(tracks)`. This is cleaner (format 0/2 have
  no conductor) but **requires an api-planning update** on the `midi` side. Flag,
  not a silent change.
- **`division` is now typed** (`{ppqn, N} | {smpte, Fps, Tpf}`) rather than a
  raw 16-bit int — handles SMPTE-division files the current code mishandles.
- Channel base, error contract, and SysEx-as-binary all match what
  `NEEDS-from-midiio.md` (R4/R6/R7) asked midilib to provide.

## 10. Open questions (carried, not blocking)

1. Should `midimsg` expose **field accessors/guards** (so LFE consumers in
   ut-proj need not include the `.hrl`), or is sharing the header across the
   family acceptable? (Leaning: share the header; add accessors only if LFE
   ergonomics demand.) Revisit in Arc 5.
2. `#channel_mode{}` vs folding modes into `#control_change{}` — kept distinct
   here; revisit if Arc 3 finds the distinction buys nothing for `midi`.
