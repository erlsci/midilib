%%% PropEr round-trip properties for the wire codec.
%%%
%%% The foundational property the audit asked for (#19): for every canonical
%%% wire message, decode(encode(M)) =:= {ok, M}. midifile gets its own
%%% read(write(Seq)) property in Arc 4.
%%%
%%% Named `prop_midibin` (not `midibin_props`) so rebar3_proper's auto-discovery
%%% — which only picks up files whose basename starts with `prop_` — finds it
%%% under the bare `proper` command the `coverage`/`check` alias invokes.
-module(prop_midibin).

-include_lib("proper/include/proper.hrl").
-include("include/midi_msg.hrl").

%%% --------------------------------------------------------------------------
%%% Generators
%%% --------------------------------------------------------------------------

chan()  -> integer(1, 16).
u7()    -> integer(0, 127).
u14()   -> integer(0, 16383).

%% Control numbers 0..119 only: 120..127 at canonical values are channel-mode
%% territory (decode returns #channel_mode{}), so they are exercised separately
%% by channel_mode_msg/0, not here.
control() -> integer(0, 119).

channel_voice_msg() ->
    oneof([
        ?LET({C, P, V}, {chan(), u7(), u7()}, #note_off{channel = C, pitch = P, velocity = V}),
        ?LET({C, P, V}, {chan(), u7(), u7()}, #note_on{channel = C, pitch = P, velocity = V}),
        ?LET({C, P, V}, {chan(), u7(), u7()}, #poly_aftertouch{channel = C, pitch = P, pressure = V}),
        ?LET({C, Pr},   {chan(), u7()},       #program_change{channel = C, program = Pr}),
        ?LET({C, Pr},   {chan(), u7()},       #channel_aftertouch{channel = C, pressure = Pr}),
        ?LET({C, V},    {chan(), u14()},      #pitch_bend{channel = C, value = V}),
        ?LET({C, Ct, V},{chan(), control(), u7()}, #control_change{channel = C, control = Ct, value = V})
    ]).

%% For all modes except mono_mode_on the canonical record carries value = 0
%% (the on/off distinction is in the atom); mono_mode_on carries a count.
channel_mode_msg() ->
    ?LET(C, chan(),
         oneof([
             #channel_mode{channel = C, mode = all_sound_off},
             #channel_mode{channel = C, mode = reset_all_controllers},
             #channel_mode{channel = C, mode = local_control_off},
             #channel_mode{channel = C, mode = local_control_on},
             #channel_mode{channel = C, mode = all_notes_off},
             #channel_mode{channel = C, mode = omni_mode_off},
             #channel_mode{channel = C, mode = omni_mode_on},
             #channel_mode{channel = C, mode = poly_mode_on},
             ?LET(N, u7(), #channel_mode{channel = C, mode = mono_mode_on, value = N})
         ])).

system_msg() ->
    oneof([
        ?LET({T, V}, {integer(0, 7), integer(0, 15)}, #mtc_quarter_frame{message_type = T, value = V}),
        ?LET(P, u14(), #song_position{position = P}),
        ?LET(S, u7(),  #song_select{song = S}),
        #tune_request{},
        #end_of_exclusive{},
        ?LET(T, oneof([clock, start, continue, stop, active_sensing, reset]),
             #realtime{type = T})
    ]).

%% SysEx data bytes are 7-bit (0..127), so they can never collide with the
%% F0/F7 framing bytes.
sysex_msg() ->
    ?LET(L, list(u7()), #sysex{data = list_to_binary(L)}).

message() ->
    frequency([
        {7, channel_voice_msg()},
        {2, channel_mode_msg()},
        {2, system_msg()},
        {1, sysex_msg()}
    ]).

%%% --------------------------------------------------------------------------
%%% Properties
%%% --------------------------------------------------------------------------

prop_roundtrip() ->
    ?FORALL(M, message(),
            begin
                {ok, Bin} = midibin:encode(M),
                {ok, M} =:= midibin:decode(Bin)
            end).

%% Encoding always yields a binary whose first byte has the high bit set
%% (a status byte), for every generated message.
prop_encode_starts_with_status_byte() ->
    ?FORALL(M, message(),
            begin
                {ok, <<First, _/binary>>} = midibin:encode(M),
                First >= 16#80
            end).
