%%% ===========================================================================
%%% midi_msg.hrl — midilib canonical message vocabulary (v0.6.0)
%%%
%%% The single message representation shared across the realtime path
%%% (midibin), the file path (midifile), and construction (midimsg). See
%%% docs/design-v0.6.0/arc02-canonical-vocabulary/DESIGN-vocabulary.md.
%%%
%%% Keystone: a `message()` is timing-free (what); an `#event{}` is a
%%% `message()` plus a delta-time (when). Channels are 1-based (1..16);
%%% conversion to/from the 0..15 wire nibble happens only inside the codecs.
%%% Records are used bare (no `{midi, _}` envelope) — they self-tag.
%%%
%%% This header is encoding-agnostic: wire/file constants live in midi.hrl.
%%% ===========================================================================

-ifndef(MIDI_MSG_HRL).
-define(MIDI_MSG_HRL, true).

%%% --- Field value types -----------------------------------------------------
-type channel()  :: 1..16.        %% canonical, 1-based
-type data7()    :: 0..127.       %% a single 7-bit data byte
-type value14bit() :: 0..16383.   %% a 14-bit value (pitch bend, song position)

%%% --- Channel-voice messages ------------------------------------------------
-record(note_off,           {channel :: channel(),
                             pitch    :: data7(),
                             velocity :: data7()}).
%% Note-On with velocity 0 is preserved as #note_on{velocity = 0}; it is NOT
%% folded to a note-off here (normalization is the consumer's job — C8/R6).
-record(note_on,            {channel :: channel(),
                             pitch    :: data7(),
                             velocity :: data7()}).
-record(poly_aftertouch,    {channel :: channel(),
                             pitch    :: data7(),
                             pressure :: data7()}).
-record(control_change,     {channel :: channel(),
                             control  :: data7(),
                             value    :: data7()}).
-record(program_change,     {channel :: channel(),
                             program  :: data7()}).
-record(channel_aftertouch, {channel :: channel(),
                             pressure :: data7()}).
-record(pitch_bend,         {channel :: channel(),
                             value    :: value14bit()}).   %% centre = 8192

%%% --- Channel-mode messages -------------------------------------------------
%% Modelled distinctly from raw CC 120-127 to preserve their semantics.
%% mono_mode_on carries the channel count in `value`; 0 otherwise.
-type channel_mode_name() :: all_sound_off
                           | reset_all_controllers
                           | local_control_off
                           | local_control_on
                           | all_notes_off
                           | omni_mode_off
                           | omni_mode_on
                           | mono_mode_on
                           | poly_mode_on.
-record(channel_mode, {channel :: channel(),
                       mode     :: channel_mode_name(),
                       value = 0 :: data7()}).

%%% --- System-common messages ------------------------------------------------
-record(mtc_quarter_frame, {message_type :: 0..7,
                            value         :: 0..15}).
-record(song_position,     {position :: value14bit()}).
-record(song_select,       {song :: data7()}).
-record(tune_request,      {}).
-record(end_of_exclusive,  {}).                          %% a lone F7

%%% --- System real-time messages ---------------------------------------------
-type realtime_type() :: clock | start | continue | stop | active_sensing | reset.
-record(realtime, {type :: realtime_type()}).

%%% --- System-exclusive messages ---------------------------------------------
%% `data` is the payload BETWEEN F0 and F7 (manufacturer id + body). The codec
%% adds/strips the F0/F7 framing. Arbitrary length (fixes the single-byte gap).
-record(sysex, {data :: binary()}).

%%% --- Meta events (Standard MIDI File only) ---------------------------------
-record(meta_sequence_number,    {value :: 0..65535}).
-record(meta_text,               {text :: unicode:chardata()}).
-record(meta_copyright,          {text :: unicode:chardata()}).
-record(meta_track_name,         {name :: unicode:chardata()}).
-record(meta_instrument_name,    {name :: unicode:chardata()}).
-record(meta_lyric,              {text :: unicode:chardata()}).
-record(meta_marker,             {text :: unicode:chardata()}).
-record(meta_cue_point,          {text :: unicode:chardata()}).
-record(meta_channel_prefix,     {channel :: channel()}).
-record(meta_set_tempo,          {usec_per_quarter :: pos_integer()}).
-record(meta_smpte_offset,       {hour      :: 0..23,
                                  minute     :: 0..59,
                                  second     :: 0..59,
                                  frame      :: 0..29,
                                  sub_frame  :: 0..99}).
%% `denominator` is the human value (e.g. 8 for 6/8); the codec converts to/from
%% the spec's 2^dd exponent. Only powers of two are representable.
-record(meta_time_signature,     {numerator                :: pos_integer(),
                                  denominator               :: pos_integer(),
                                  clocks_per_click          :: pos_integer(),
                                  notated_32nd_per_quarter  :: pos_integer()}).
-record(meta_key_signature,      {key  :: -7..7,           %% sharps(+)/flats(-)
                                  mode :: major | minor}).
-record(meta_sequencer_specific, {data :: binary()}).
-record(meta_end_of_track,       {}).
%% Preserves any meta type we do not model, for lossless round-trip.
-record(meta_unknown,            {type :: 0..127,
                                  data :: binary()}).

%%% --- Container types --------------------------------------------------------
-record(event, {delta   :: non_neg_integer(),            %% ticks since prev event
                message  :: message()}).
-record(track, {events :: [#event{}]}).
%% No explicit conductor-track field: for format 1 it is simply hd(tracks).
-record(seq,   {format   :: 0..2,
                division :: {ppqn, pos_integer()}
                          | {smpte, pos_integer(), pos_integer()},
                tracks   :: [#track{}]}).

%%% --- Union types (the contract surface) ------------------------------------
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

-type message() :: channel_message() | system_message() | meta_message().
-type event()   :: #event{}.

-endif. %% MIDI_MSG_HRL
