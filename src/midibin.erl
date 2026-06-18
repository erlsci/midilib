%%% ===========================================================================
%%% midibin — binary <-> canonical-message codec for live (wire) MIDI.
%%%
%%% Works in the canonical vocabulary (include/midi_msg.hrl): bare records, no
%%% `{midi, _}` envelope, 1-based channels at the API boundary (the 0..15 wire
%%% nibble is internal). One complete message per decode/encode call — no
%%% running-status expansion, no stream reassembly (the transport contract the
%%% family relies on, NEEDS R1).
%%% ===========================================================================
-module(midibin).
%% Binary <-> canonical-message MIDI codec. decode/1 turns one complete wire
%% message into a canonical record; encode/1 does the reverse. encode_batch/1 and
%% decode_batch/1 operate on lists. Failures are {error, midierrs:reason()};
%% render with midierrs:format_error/1.
%% Kept portable: the v0.6.0 support floor is OTP 22-29 (row 12), so the
%% OTP-27+ -moduledoc/-doc attributes are intentionally not used; module/function
%% docs stay in %%-comments and -spec.

-export([decode/1, encode/1, encode_batch/1, decode_batch/1]).

-include("include/midi_msg.hrl").

%% Guard helpers — keep encode total and prevent silent bit-syntax truncation.
-define(is_chan(X),  (is_integer(X) andalso X >= 1 andalso X =< 16)).
-define(is_u7(X),    (is_integer(X) andalso X >= 0 andalso X =< 127)).
-define(is_u14(X),   (is_integer(X) andalso X >= 0 andalso X =< 16383)).

%%% ===========================================================================
%%% DECODE
%%% ===========================================================================
%% Decode one complete wire message into a canonical record. Input is always
%% wire bytes; unrecognised bytes are {error, {unknown, Bin}} (DESIGN §6). A
%% non-binary argument is a caller bug and crashes (EH-05) — `non_midi` is an
%% encode-only reason, not a decode outcome.
-spec decode(binary()) -> {ok, message()} | {error, {unknown, binary()}}.

%%% --- Channel-voice ---------------------------------------------------------
decode(<<1:1, 0:3, Ch:4, 0:1, Pitch:7, 0:1, Vel:7>>) ->
    {ok, #note_off{channel = Ch + 1, pitch = Pitch, velocity = Vel}};
decode(<<1:1, 1:3, Ch:4, 0:1, Pitch:7, 0:1, Vel:7>>) ->
    %% Velocity 0 is preserved as note_on vel 0 (NOT folded — C8/R6).
    {ok, #note_on{channel = Ch + 1, pitch = Pitch, velocity = Vel}};
decode(<<1:1, 2:3, Ch:4, 0:1, Pitch:7, 0:1, Pres:7>>) ->
    {ok, #poly_aftertouch{channel = Ch + 1, pitch = Pitch, pressure = Pres}};
decode(<<1:1, 4:3, Ch:4, 0:1, Prog:7>>) ->
    {ok, #program_change{channel = Ch + 1, program = Prog}};
decode(<<1:1, 5:3, Ch:4, 0:1, Pres:7>>) ->
    {ok, #channel_aftertouch{channel = Ch + 1, pressure = Pres}};
decode(<<1:1, 6:3, Ch:4, 0:1, Lsb:7, 0:1, Msb:7>>) ->
    {ok, #pitch_bend{channel = Ch + 1, value = (Msb bsl 7) + Lsb}};

%%% --- Channel-mode (controllers 120-127 at canonical values) ----------------
decode(<<1:1, 3:3, Ch:4, 0:1, 120:7, 0:1, 0:7>>) ->
    {ok, #channel_mode{channel = Ch + 1, mode = all_sound_off}};
decode(<<1:1, 3:3, Ch:4, 0:1, 121:7, 0:1, 0:7>>) ->
    {ok, #channel_mode{channel = Ch + 1, mode = reset_all_controllers}};
decode(<<1:1, 3:3, Ch:4, 0:1, 122:7, 0:1, 0:7>>) ->
    {ok, #channel_mode{channel = Ch + 1, mode = local_control_off}};
decode(<<1:1, 3:3, Ch:4, 0:1, 122:7, 0:1, 127:7>>) ->
    {ok, #channel_mode{channel = Ch + 1, mode = local_control_on}};
decode(<<1:1, 3:3, Ch:4, 0:1, 123:7, 0:1, 0:7>>) ->
    {ok, #channel_mode{channel = Ch + 1, mode = all_notes_off}};
decode(<<1:1, 3:3, Ch:4, 0:1, 124:7, 0:1, 0:7>>) ->
    {ok, #channel_mode{channel = Ch + 1, mode = omni_mode_off}};
decode(<<1:1, 3:3, Ch:4, 0:1, 125:7, 0:1, 0:7>>) ->
    {ok, #channel_mode{channel = Ch + 1, mode = omni_mode_on}};
decode(<<1:1, 3:3, Ch:4, 0:1, 126:7, 0:1, N:7>>) ->
    {ok, #channel_mode{channel = Ch + 1, mode = mono_mode_on, value = N}};
decode(<<1:1, 3:3, Ch:4, 0:1, 127:7, 0:1, 0:7>>) ->
    {ok, #channel_mode{channel = Ch + 1, mode = poly_mode_on}};

%%% --- Control change (controllers 0-119; and 120-127 with non-canonical -----
%%%     values — faithful to the bytes, documented C5 policy) ------------------
decode(<<1:1, 3:3, Ch:4, 0:1, Ctrl:7, 0:1, Val:7>>) ->
    {ok, #control_change{channel = Ch + 1, control = Ctrl, value = Val}};

%%% --- System-common ---------------------------------------------------------
decode(<<1:1, 7:3, 1:4, 0:1, MsgType:3, Val:4>>) ->
    {ok, #mtc_quarter_frame{message_type = MsgType, value = Val}};
decode(<<1:1, 7:3, 2:4, 0:1, Lsb:7, 0:1, Msb:7>>) ->
    {ok, #song_position{position = (Msb bsl 7) + Lsb}};
decode(<<1:1, 7:3, 3:4, 0:1, Song:7>>) ->
    {ok, #song_select{song = Song}};
decode(<<1:1, 7:3, 6:4>>) ->
    {ok, #tune_request{}};
decode(<<1:1, 7:3, 7:4>>) ->
    {ok, #end_of_exclusive{}};

%%% --- System real-time ------------------------------------------------------
decode(<<16#F8>>) -> {ok, #realtime{type = clock}};
decode(<<16#FA>>) -> {ok, #realtime{type = start}};
decode(<<16#FB>>) -> {ok, #realtime{type = continue}};
decode(<<16#FC>>) -> {ok, #realtime{type = stop}};
decode(<<16#FE>>) -> {ok, #realtime{type = active_sensing}};
decode(<<16#FF>>) -> {ok, #realtime{type = reset}};

%%% --- System-exclusive (arbitrary length; F0 ... F7) ------------------------
decode(<<16#F0, Rest/binary>>) ->
    Size = byte_size(Rest),
    case Size > 0 andalso binary:at(Rest, Size - 1) =:= 16#F7 of
        true  -> {ok, #sysex{data = binary:part(Rest, 0, Size - 1)}};
        false -> {error, {unknown, <<16#F0, Rest/binary>>}}
    end;

%%% --- Fallback --------------------------------------------------------------
decode(Bin) when is_binary(Bin) -> {error, {unknown, Bin}}.

%%% ===========================================================================
%%% ENCODE
%%% ===========================================================================
%% Encode one canonical message into its wire bytes.
-spec encode(message() | term()) -> {ok, binary()} | {error, midierrs:reason()}.

%%% --- Channel-voice ---------------------------------------------------------
encode(#note_off{channel = Ch, pitch = P, velocity = V})
  when ?is_chan(Ch), ?is_u7(P), ?is_u7(V) ->
    {ok, <<1:1, 0:3, (Ch - 1):4, 0:1, P:7, 0:1, V:7>>};
encode(#note_on{channel = Ch, pitch = P, velocity = V})
  when ?is_chan(Ch), ?is_u7(P), ?is_u7(V) ->
    {ok, <<1:1, 1:3, (Ch - 1):4, 0:1, P:7, 0:1, V:7>>};
encode(#poly_aftertouch{channel = Ch, pitch = P, pressure = Pr})
  when ?is_chan(Ch), ?is_u7(P), ?is_u7(Pr) ->
    {ok, <<1:1, 2:3, (Ch - 1):4, 0:1, P:7, 0:1, Pr:7>>};
encode(#program_change{channel = Ch, program = Prog})
  when ?is_chan(Ch), ?is_u7(Prog) ->
    {ok, <<1:1, 4:3, (Ch - 1):4, 0:1, Prog:7>>};
encode(#channel_aftertouch{channel = Ch, pressure = Pr})
  when ?is_chan(Ch), ?is_u7(Pr) ->
    {ok, <<1:1, 5:3, (Ch - 1):4, 0:1, Pr:7>>};
encode(#pitch_bend{channel = Ch, value = Val})
  when ?is_chan(Ch), ?is_u14(Val) ->
    Lsb = Val band 127,
    Msb = (Val bsr 7) band 127,
    {ok, <<1:1, 6:3, (Ch - 1):4, 0:1, Lsb:7, 0:1, Msb:7>>};

%%% --- Channel-mode ----------------------------------------------------------
encode(#channel_mode{channel = Ch, mode = Mode, value = Val})
  when ?is_chan(Ch), ?is_u7(Val) ->
    %% Mode is a channel_mode_name() (a closed union); an out-of-contract atom
    %% is a caller bug and crashes here (EH-05) rather than returning a value.
    {Ctrl, OutVal} = mode_control(Mode, Val),
    {ok, <<1:1, 3:3, (Ch - 1):4, 0:1, Ctrl:7, 0:1, OutVal:7>>};

%%% --- Control change --------------------------------------------------------
encode(#control_change{channel = Ch, control = C, value = V})
  when ?is_chan(Ch), ?is_u7(C), ?is_u7(V) ->
    {ok, <<1:1, 3:3, (Ch - 1):4, 0:1, C:7, 0:1, V:7>>};

%%% --- System-common ---------------------------------------------------------
encode(#mtc_quarter_frame{message_type = T, value = V})
  when is_integer(T), T >= 0, T =< 7, is_integer(V), V >= 0, V =< 15 ->
    {ok, <<1:1, 7:3, 1:4, 0:1, T:3, V:4>>};
encode(#song_position{position = Pos}) when ?is_u14(Pos) ->
    Lsb = Pos band 127,
    Msb = (Pos bsr 7) band 127,
    {ok, <<1:1, 7:3, 2:4, 0:1, Lsb:7, 0:1, Msb:7>>};
encode(#song_select{song = S}) when ?is_u7(S) ->
    {ok, <<1:1, 7:3, 3:4, 0:1, S:7>>};
encode(#tune_request{}) ->
    {ok, <<1:1, 7:3, 6:4>>};
encode(#end_of_exclusive{}) ->
    {ok, <<1:1, 7:3, 7:4>>};

%%% --- System real-time ------------------------------------------------------
encode(#realtime{type = clock})          -> {ok, <<16#F8>>};
encode(#realtime{type = start})          -> {ok, <<16#FA>>};
encode(#realtime{type = continue})       -> {ok, <<16#FB>>};
encode(#realtime{type = stop})           -> {ok, <<16#FC>>};
encode(#realtime{type = active_sensing}) -> {ok, <<16#FE>>};
encode(#realtime{type = reset})          -> {ok, <<16#FF>>};

%%% --- System-exclusive ------------------------------------------------------
encode(#sysex{data = D}) when is_binary(D) ->
    {ok, <<16#F0, D/binary, 16#F7>>};

%%% --- Fallbacks -------------------------------------------------------------
%% A well-formed-but-non-wire message (any #meta_*{}) or an out-of-range record
%% lands here; non-records are simply not MIDI.
encode(Term) when is_tuple(Term) -> {error, {unsupported, Term}};
encode(_)                        -> {error, non_midi}.

%%% ===========================================================================
%%% BATCH (list in, list out — no {midi,{batch,_}} envelope; O(n))
%%% ===========================================================================
%% Encode a list of messages to a list of binaries. Short-circuits on the first
%% un-encodable message, appending a single trailing {error, _} (the behaviour
%% `midi` relies on, NEEDS R9).
-spec encode_batch([message()]) -> [binary() | {error, midierrs:reason()}].
encode_batch(Msgs) -> encode_batch(Msgs, []).

encode_batch([], Acc) ->
    lists:reverse(Acc);
encode_batch([Msg | Rest], Acc) ->
    case encode(Msg) of
        {ok, Bin}     -> encode_batch(Rest, [Bin | Acc]);
        {error, _} = E -> lists:reverse([E | Acc])
    end.

%% Decode a list of complete-message binaries; one result per element.
-spec decode_batch([binary()]) -> [{ok, message()} | {error, term()}].
decode_batch(Bins) ->
    [decode(B) || B <- Bins].

%%% ===========================================================================
%%% Internal
%%% ===========================================================================

%% Map a channel-mode name (+ its value, used by mono_mode_on) to the wire
%% controller number and the value byte to emit. Total over channel_mode_name();
%% an out-of-contract atom has no clause and crashes (a caller bug, EH-05).
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
