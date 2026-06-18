%%% ===========================================================================
%%% midi_codec — pure, framing-free message <-> bytes core.
%%%
%%% The single home for the canonical-vocabulary byte mapping that both framers
%%% (midibin, wire; midifile, file) sit on, so a note-on (or any message) can
%%% never be laid out one way on read and another on write — the exact drift the
%%% v0.6.0 audit found.
%%%
%%% This module is STATELESS and I/O-FREE: no file:/io:, no process dictionary,
%%% no running-status, no delta-time, no chunk framing. It maps
%%%
%%%   channel-voice / channel-mode / system message  <->  {Status, DataBytes}
%%%   meta record                                     <->  {MetaType, Payload}
%%%
%%% and exposes data_length/1, the table the file framer needs to apply running
%%% status. F0/F7 SysEx *framing* and FF/VLQ meta *framing* belong to the
%%% framers; this module owns only the value mapping inside that framing.
%%%
%%% It carries the invariants that used to be scattered across the codecs:
%%%   * 1-based channels (the 0..15 wire nibble <-> 1..16 ±1 lives here, once);
%%%   * the C5 channel-mode policy (controllers 120-127 at canonical values are
%%%     #channel_mode{}, every other value is a faithful #control_change{});
%%%   * no vel-0 fold (0x9n note 0 stays #note_on{velocity = 0} — normalization
%%%     is the consumer's job, C8/R6);
%%%   * the meta value conversions (tempo µs/qn, time-sig 2^dd, key-sig,
%%%     #meta_unknown{} verbatim round-trip).
%%%
%%% Kept portable: the v0.6.0 support floor is OTP 22-29, so the OTP-27+
%%% -moduledoc/-doc attributes are intentionally not used (midibin's precedent).
%%% ===========================================================================
-module(midi_codec).

-export([decode_message/2, encode_message/1, data_length/1,
         decode_meta/2, encode_meta/1]).

-include("include/midi_msg.hrl").
-include("include/midi.hrl").

%% Guard helpers — keep encode total and prevent silent bit-syntax truncation.
-define(is_chan(X), (is_integer(X) andalso X >= 1 andalso X =< 16)).
-define(is_u7(X),   (is_integer(X) andalso X >= 0 andalso X =< 127)).
-define(is_u14(X),  (is_integer(X) andalso X >= 0 andalso X =< 16383)).

%%% ===========================================================================
%%% DECODE: {Status, Data} -> message()
%%% ===========================================================================
%% Decode one channel-voice/channel-mode/system message from its status byte
%% and (already-sized) data bytes. The caller (a framer) has done the framing:
%% it knows how many data bytes the status consumes (see data_length/1) and
%% supplies exactly those. SysEx (F0) and meta (FF) framing are the framer's;
%% this never sees them on a well-formed path. Unrecognised input is a value,
%% {error, {unknown, Bytes}}, with Bytes the reconstructed status+data — so a
%% framer can surface the offending bytes unchanged.
-spec decode_message(0..255, binary()) ->
          {ok, channel_message() | system_message()}
        | {error, midierrs:reason()}.

%%% --- Channel-voice ---------------------------------------------------------
decode_message(Status, <<0:1, Pitch:7, 0:1, Vel:7>>)
  when Status band 16#F0 =:= 16#80 ->
    {ok, #note_off{channel = chan(Status), pitch = Pitch, velocity = Vel}};
decode_message(Status, <<0:1, Pitch:7, 0:1, Vel:7>>)
  when Status band 16#F0 =:= 16#90 ->
    %% Velocity 0 is preserved as note_on vel 0 (NOT folded — C8/R6).
    {ok, #note_on{channel = chan(Status), pitch = Pitch, velocity = Vel}};
decode_message(Status, <<0:1, Pitch:7, 0:1, Pres:7>>)
  when Status band 16#F0 =:= 16#A0 ->
    {ok, #poly_aftertouch{channel = chan(Status), pitch = Pitch, pressure = Pres}};
decode_message(Status, <<0:1, Ctrl:7, 0:1, Val:7>>)
  when Status band 16#F0 =:= 16#B0 ->
    decode_controller(chan(Status), Ctrl, Val);
decode_message(Status, <<0:1, Prog:7>>)
  when Status band 16#F0 =:= 16#C0 ->
    {ok, #program_change{channel = chan(Status), program = Prog}};
decode_message(Status, <<0:1, Pres:7>>)
  when Status band 16#F0 =:= 16#D0 ->
    {ok, #channel_aftertouch{channel = chan(Status), pressure = Pres}};
decode_message(Status, <<0:1, Lsb:7, 0:1, Msb:7>>)
  when Status band 16#F0 =:= 16#E0 ->
    {ok, #pitch_bend{channel = chan(Status), value = (Msb bsl 7) + Lsb}};

%%% --- System-common ---------------------------------------------------------
decode_message(16#F1, <<0:1, MsgType:3, Val:4>>) ->
    {ok, #mtc_quarter_frame{message_type = MsgType, value = Val}};
decode_message(?STATUS_SONG_POINTER, <<0:1, Lsb:7, 0:1, Msb:7>>) ->
    {ok, #song_position{position = (Msb bsl 7) + Lsb}};
decode_message(?STATUS_SONG_SELECT, <<0:1, Song:7>>) ->
    {ok, #song_select{song = Song}};
decode_message(?STATUS_TUNE_REQUEST, <<>>) ->
    {ok, #tune_request{}};
decode_message(?STATUS_EOX, <<>>) ->
    {ok, #end_of_exclusive{}};

%%% --- System real-time ------------------------------------------------------
decode_message(?STATUS_CLOCK, <<>>)        -> {ok, #realtime{type = clock}};
decode_message(?STATUS_START, <<>>)        -> {ok, #realtime{type = start}};
decode_message(?STATUS_CONTINUE, <<>>)     -> {ok, #realtime{type = continue}};
decode_message(?STATUS_STOP, <<>>)         -> {ok, #realtime{type = stop}};
decode_message(?STATUS_ACTIVE_SENSE, <<>>) -> {ok, #realtime{type = active_sensing}};
decode_message(?STATUS_SYSTEM_RESET, <<>>) -> {ok, #realtime{type = reset}};

%%% --- Fallback --------------------------------------------------------------
%% Not a status byte we map, or the wrong data length for one that we do:
%% report the reconstructed bytes so the framer can surface them verbatim.
decode_message(Status, Data) when is_integer(Status), Status >= 0, Status =< 255 ->
    {error, {unknown, <<Status, Data/binary>>}}.

%% Channel-mode vs control-change — the C5 policy, in one place. Controllers
%% 120-127 at their canonical value are channel-mode messages; the SAME numbers
%% with any other value are faithful control changes (FP-12: the specific
%% mode clauses precede the generic CC clause, and nothing below is shadowed).
-spec decode_controller(channel(), data7(), data7()) -> {ok, channel_message()}.
decode_controller(Ch, 120, 0)   -> {ok, #channel_mode{channel = Ch, mode = all_sound_off}};
decode_controller(Ch, 121, 0)   -> {ok, #channel_mode{channel = Ch, mode = reset_all_controllers}};
decode_controller(Ch, 122, 0)   -> {ok, #channel_mode{channel = Ch, mode = local_control_off}};
decode_controller(Ch, 122, 127) -> {ok, #channel_mode{channel = Ch, mode = local_control_on}};
decode_controller(Ch, 123, 0)   -> {ok, #channel_mode{channel = Ch, mode = all_notes_off}};
decode_controller(Ch, 124, 0)   -> {ok, #channel_mode{channel = Ch, mode = omni_mode_off}};
decode_controller(Ch, 125, 0)   -> {ok, #channel_mode{channel = Ch, mode = omni_mode_on}};
decode_controller(Ch, 126, N)   -> {ok, #channel_mode{channel = Ch, mode = mono_mode_on, value = N}};
decode_controller(Ch, 127, 0)   -> {ok, #channel_mode{channel = Ch, mode = poly_mode_on}};
decode_controller(Ch, Ctrl, Val) ->
    {ok, #control_change{channel = Ch, control = Ctrl, value = Val}}.

%%% ===========================================================================
%%% ENCODE: message() -> {Status, Data}
%%% ===========================================================================
%% Encode one channel/system message into its status byte and data bytes; the
%% framer adds whatever wrapping its transport needs. SysEx framing (F0 ... F7
%% or F0 <vlq>) and meta framing are the framer's job, so #sysex{} and #meta_*{}
%% are not encoded here — a #meta_*{} (a tuple) lands on the {unsupported, _}
%% fallback, matching the wire codec's "meta is file-only" contract.
-spec encode_message(message() | term()) ->
          {ok, {0..255, binary()}} | {error, midierrs:reason()}.

%%% --- Channel-voice ---------------------------------------------------------
encode_message(#note_off{channel = Ch, pitch = P, velocity = V})
  when ?is_chan(Ch), ?is_u7(P), ?is_u7(V) ->
    {ok, {16#80 bor nibble(Ch), <<0:1, P:7, 0:1, V:7>>}};
encode_message(#note_on{channel = Ch, pitch = P, velocity = V})
  when ?is_chan(Ch), ?is_u7(P), ?is_u7(V) ->
    {ok, {16#90 bor nibble(Ch), <<0:1, P:7, 0:1, V:7>>}};
encode_message(#poly_aftertouch{channel = Ch, pitch = P, pressure = Pr})
  when ?is_chan(Ch), ?is_u7(P), ?is_u7(Pr) ->
    {ok, {16#A0 bor nibble(Ch), <<0:1, P:7, 0:1, Pr:7>>}};
encode_message(#program_change{channel = Ch, program = Prog})
  when ?is_chan(Ch), ?is_u7(Prog) ->
    {ok, {16#C0 bor nibble(Ch), <<0:1, Prog:7>>}};
encode_message(#channel_aftertouch{channel = Ch, pressure = Pr})
  when ?is_chan(Ch), ?is_u7(Pr) ->
    {ok, {16#D0 bor nibble(Ch), <<0:1, Pr:7>>}};
encode_message(#pitch_bend{channel = Ch, value = Val})
  when ?is_chan(Ch), ?is_u14(Val) ->
    {ok, {16#E0 bor nibble(Ch), <<0:1, (Val band 127):7, 0:1, (Val bsr 7):7>>}};

%%% --- Channel-mode ----------------------------------------------------------
encode_message(#channel_mode{channel = Ch, mode = Mode, value = Val})
  when ?is_chan(Ch), ?is_u7(Val) ->
    %% Mode is a channel_mode_name() (a closed union); an out-of-contract atom
    %% has no mode_control/2 clause and crashes here (a caller bug, EH-05).
    {Ctrl, OutVal} = mode_control(Mode, Val),
    {ok, {16#B0 bor nibble(Ch), <<0:1, Ctrl:7, 0:1, OutVal:7>>}};

%%% --- Control change --------------------------------------------------------
encode_message(#control_change{channel = Ch, control = C, value = V})
  when ?is_chan(Ch), ?is_u7(C), ?is_u7(V) ->
    {ok, {16#B0 bor nibble(Ch), <<0:1, C:7, 0:1, V:7>>}};

%%% --- System-common ---------------------------------------------------------
encode_message(#mtc_quarter_frame{message_type = T, value = V})
  when is_integer(T), T >= 0, T =< 7, is_integer(V), V >= 0, V =< 15 ->
    {ok, {16#F1, <<0:1, T:3, V:4>>}};
encode_message(#song_position{position = Pos}) when ?is_u14(Pos) ->
    {ok, {?STATUS_SONG_POINTER, <<0:1, (Pos band 127):7, 0:1, (Pos bsr 7):7>>}};
encode_message(#song_select{song = S}) when ?is_u7(S) ->
    {ok, {?STATUS_SONG_SELECT, <<0:1, S:7>>}};
encode_message(#tune_request{})     -> {ok, {?STATUS_TUNE_REQUEST, <<>>}};
encode_message(#end_of_exclusive{}) -> {ok, {?STATUS_EOX, <<>>}};

%%% --- System real-time ------------------------------------------------------
encode_message(#realtime{type = clock})          -> {ok, {?STATUS_CLOCK, <<>>}};
encode_message(#realtime{type = start})          -> {ok, {?STATUS_START, <<>>}};
encode_message(#realtime{type = continue})       -> {ok, {?STATUS_CONTINUE, <<>>}};
encode_message(#realtime{type = stop})           -> {ok, {?STATUS_STOP, <<>>}};
encode_message(#realtime{type = active_sensing}) -> {ok, {?STATUS_ACTIVE_SENSE, <<>>}};
encode_message(#realtime{type = reset})          -> {ok, {?STATUS_SYSTEM_RESET, <<>>}};

%%% --- Fallbacks -------------------------------------------------------------
%% A meta record, a #sysex{}, or an out-of-range channel record is a tuple but
%% not a wire-encodable body here; a non-record is simply not MIDI.
encode_message(Term) when is_tuple(Term) -> {error, {unsupported, Term}};
encode_message(_)                        -> {error, non_midi}.

%%% ===========================================================================
%%% DATA LENGTH — how many data bytes a status byte consumes
%%% ===========================================================================
%% The table the file framer needs to apply running status: given a recovered
%% status byte, how many data bytes follow. `variable` flags F0/F7, whose length
%% is carried by the framing (a VLQ in a file), not by the status byte.
-spec data_length(0..255) -> 0 | 1 | 2 | variable.
data_length(S) when S >= 16#80, S =< 16#BF -> 2;  %% note off/on, poly press, control
data_length(S) when S >= 16#C0, S =< 16#DF -> 1;  %% program change, channel pressure
data_length(S) when S >= 16#E0, S =< 16#EF -> 2;  %% pitch bend
data_length(?STATUS_SYSEX)        -> variable;
data_length(16#F1)                -> 1;           %% MTC quarter frame
data_length(?STATUS_SONG_POINTER) -> 2;
data_length(?STATUS_SONG_SELECT)  -> 1;
data_length(?STATUS_TUNE_REQUEST) -> 0;
data_length(?STATUS_EOX)          -> variable;    %% lone F7 / sysex escape
data_length(S) when S >= 16#F8, S =< 16#FF -> 0;  %% system real-time (incl. FF)
data_length(S) when S >= 16#F4, S =< 16#F5 -> 0.  %% undefined system-common: no data

%%% ===========================================================================
%%% META: {MetaType, Payload} <-> meta_message()
%%% ===========================================================================
%% Decode a meta event body (the bytes after `FF Type <vlq-len>`) into a record.
%% A modelled type with an unexpected payload length, or any unmodelled type,
%% falls through to #meta_unknown{} so nothing is lost (verbatim round-trip);
%% the mapping is therefore total over 0..127 and never returns an error. A type
%% byte >= 128 is malformed (the SMF type byte is 7-bit) and has no clause — it
%% crashes (let-it-crash, R4) rather than minting an out-of-range record.
-spec decode_meta(0..127, binary()) -> {ok, meta_message()}.
decode_meta(?META_SEQ_NUM, <<Num:16>>) ->
    {ok, #meta_sequence_number{value = Num}};
decode_meta(?META_TEXT, Payload) ->
    {ok, #meta_text{text = Payload}};
decode_meta(?META_COPYRIGHT, Payload) ->
    {ok, #meta_copyright{text = Payload}};
decode_meta(?META_SEQ_NAME, Payload) ->
    {ok, #meta_track_name{name = Payload}};
decode_meta(?META_INSTRUMENT, Payload) ->
    {ok, #meta_instrument_name{name = Payload}};
decode_meta(?META_LYRIC, Payload) ->
    {ok, #meta_lyric{text = Payload}};
decode_meta(?META_MARKER, Payload) ->
    {ok, #meta_marker{text = Payload}};
decode_meta(?META_CUE, Payload) ->
    {ok, #meta_cue_point{text = Payload}};
decode_meta(?META_MIDI_CHAN_PREFIX, <<Ch:8>>) when Ch =< 15 ->
    {ok, #meta_channel_prefix{channel = Ch + 1}};
decode_meta(?META_TRACK_END, <<>>) ->
    {ok, #meta_end_of_track{}};
decode_meta(?META_SET_TEMPO, <<Usec:24>>) ->
    {ok, #meta_set_tempo{usec_per_quarter = Usec}};
decode_meta(?META_SMPTE, <<H:8, M:8, S:8, F:8, Sf:8>>)
  when H =< 23, M =< 59, S =< 59, F =< 29, Sf =< 99 ->
    {ok, #meta_smpte_offset{hour = H, minute = M, second = S,
                            frame = F, sub_frame = Sf}};
decode_meta(?META_TIME_SIG, <<Num:8, DD:8, Clocks:8, Notated:8>>)
  when Num >= 1, DD =< 31, Clocks >= 1, Notated >= 1 ->
    {ok, #meta_time_signature{numerator = Num,
                              denominator = 1 bsl DD,
                              clocks_per_click = Clocks,
                              notated_32nd_per_quarter = Notated}};
decode_meta(?META_KEY_SIG, <<Key:8/signed, Mode:8>>)
  when Key >= -7, Key =< 7, Mode =< 1 ->
    {ok, #meta_key_signature{key = Key, mode = key_mode(Mode)}};
decode_meta(?META_SEQUENCER_SPECIFIC, Payload) ->
    {ok, #meta_sequencer_specific{data = Payload}};
decode_meta(Type, Payload) when is_integer(Type), Type >= 0, Type =< 127 ->
    {ok, #meta_unknown{type = Type, data = Payload}}.

%% Encode a meta record into its {type byte, payload bytes}; the framer prepends
%% `FF Type <vlq(byte_size(Payload))>`. Total over the modelled records except
%% time-signature, whose denominator must be a power of two — a non-power-of-two
%% is a structured {error, {bad_value, denominator, D}} (no silent rounding).
-spec encode_meta(meta_message()) -> {ok, {0..127, binary()}} | {error, midierrs:reason()}.
encode_meta(#meta_sequence_number{value = Num}) when Num >= 0, Num =< 65535 ->
    {ok, {?META_SEQ_NUM, <<Num:16>>}};
encode_meta(#meta_text{text = Text}) ->
    {ok, {?META_TEXT, text_bin(Text)}};
encode_meta(#meta_copyright{text = Text}) ->
    {ok, {?META_COPYRIGHT, text_bin(Text)}};
encode_meta(#meta_track_name{name = Name}) ->
    {ok, {?META_SEQ_NAME, text_bin(Name)}};
encode_meta(#meta_instrument_name{name = Name}) ->
    {ok, {?META_INSTRUMENT, text_bin(Name)}};
encode_meta(#meta_lyric{text = Text}) ->
    {ok, {?META_LYRIC, text_bin(Text)}};
encode_meta(#meta_marker{text = Text}) ->
    {ok, {?META_MARKER, text_bin(Text)}};
encode_meta(#meta_cue_point{text = Text}) ->
    {ok, {?META_CUE, text_bin(Text)}};
encode_meta(#meta_channel_prefix{channel = Ch}) when ?is_chan(Ch) ->
    {ok, {?META_MIDI_CHAN_PREFIX, <<(Ch - 1):8>>}};
encode_meta(#meta_end_of_track{}) ->
    {ok, {?META_TRACK_END, <<>>}};
encode_meta(#meta_set_tempo{usec_per_quarter = Usec})
  when is_integer(Usec), Usec >= 0, Usec =< 16#FFFFFF ->
    {ok, {?META_SET_TEMPO, <<Usec:24>>}};
encode_meta(#meta_smpte_offset{hour = H, minute = M, second = S,
                               frame = F, sub_frame = Sf})
  when H =< 23, M =< 59, S =< 59, F =< 29, Sf =< 99 ->
    {ok, {?META_SMPTE, <<H:8, M:8, S:8, F:8, Sf:8>>}};
encode_meta(#meta_time_signature{numerator = Num, denominator = Denom,
                                 clocks_per_click = Clocks,
                                 notated_32nd_per_quarter = Notated})
  when Num >= 1, Num =< 255, Clocks >= 1, Clocks =< 255,
       Notated >= 1, Notated =< 255 ->
    case denom_to_dd(Denom) of
        {ok, DD}       -> {ok, {?META_TIME_SIG, <<Num:8, DD:8, Clocks:8, Notated:8>>}};
        {error, _} = E -> E
    end;
encode_meta(#meta_key_signature{key = Key, mode = Mode})
  when Key >= -7, Key =< 7 ->
    {ok, {?META_KEY_SIG, <<Key:8/signed, (mode_key(Mode)):8>>}};
encode_meta(#meta_sequencer_specific{data = Data}) when is_binary(Data) ->
    {ok, {?META_SEQUENCER_SPECIFIC, Data}};
encode_meta(#meta_unknown{type = Type, data = Data})
  when is_integer(Type), Type >= 0, Type =< 127, is_binary(Data) ->
    {ok, {Type, Data}}.

%%% ===========================================================================
%%% Internal
%%% ===========================================================================

%% 0..15 wire nibble of a channel status byte -> 1..16 canonical channel.
-spec chan(0..255) -> channel().
chan(Status) -> (Status band 16#0F) + 1.

%% 1..16 canonical channel -> 0..15 wire nibble.
-spec nibble(channel()) -> 0..15.
nibble(Ch) -> Ch - 1.

-spec key_mode(0 | 1) -> major | minor.
key_mode(0) -> major;
key_mode(1) -> minor.

-spec mode_key(major | minor) -> 0 | 1.
mode_key(major) -> 0;
mode_key(minor) -> 1.

%% Map a channel-mode name (+ value, used by mono_mode_on) to the wire
%% controller number and the value byte to emit. Total over channel_mode_name().
-spec mode_control(channel_mode_name(), 0..127) -> {0..127, 0..127}.
mode_control(all_sound_off, _)         -> {120, 0};
mode_control(reset_all_controllers, _) -> {121, 0};
mode_control(local_control_off, _)     -> {122, 0};
mode_control(local_control_on, _)      -> {122, 127};
mode_control(all_notes_off, _)         -> {123, 0};
mode_control(omni_mode_off, _)         -> {124, 0};
mode_control(omni_mode_on, _)          -> {125, 0};
mode_control(mono_mode_on, N)          -> {126, N};
mode_control(poly_mode_on, _)          -> {127, 0}.

%% Human time-signature denominator (a power of two) -> the spec's 2^dd exponent.
-spec denom_to_dd(pos_integer()) -> {ok, 0..31} | {error, midierrs:reason()}.
denom_to_dd(Denom) when is_integer(Denom), Denom >= 1 ->
    denom_to_dd(Denom, 0);
denom_to_dd(Denom) ->
    {error, {bad_value, denominator, Denom}}.

denom_to_dd(1, DD) ->
    {ok, DD};
denom_to_dd(N, DD) when N rem 2 =:= 0, DD < 31 ->
    denom_to_dd(N div 2, DD + 1);
denom_to_dd(Denom, _) ->
    %% Not reachable by halving to 1 within 31 steps => not a power of two.
    {error, {bad_value, denominator, Denom}}.

%% Meta text fields accept a binary or any unicode:chardata(); emit raw bytes.
-spec text_bin(unicode:chardata()) -> binary().
text_bin(Bin) when is_binary(Bin) -> Bin;
text_bin(Chardata)                -> unicode:characters_to_binary(Chardata).
