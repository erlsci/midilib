-module(midibin_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("midilib/include/errors.hrl").

%%% The following tests perform round-trip operations: msg -> encode -> decode -> msg

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Channel Voice Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aftertouch_test() ->
    Msg1 = {midi, {aftertouch, [{channel, 11},
                                {pressure, 32}]}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

note_off_test() ->
    Msg1 = {midi, {note_off, [{channel, 11},
                              {pitch, 60},
                              {velocity, 32}]}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

note_on_test() ->
    Msg1 = {midi, {note_on, [{channel, 11},
                             {pitch, 60},
                             {velocity, 32}]}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

poly_aftertouch_test() ->
    Msg1 = {midi, {poly_aftertouch, [{channel, 11},
                                     {pitch, 48},
                                     {pressure, 64}]}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

cc_test() ->
    true.

program_change_test() ->
    true.

pitch_bend_test() ->
    Msg1 = {midi, {pitch_bend, [{channel, 11},
                                {value, 16000}]}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Channel Mode Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_sound_off_test() ->
    true.

reset_all_test() ->
    true.

notes_off_test() ->
    true.

omni_mode_off_test() ->
    true.

omni_mode_on_test() ->
    true.

poly_mode_off_test() ->
    true.

poly_mode_on_test() ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

time_code_quarter_frame_test() ->
    true.

song_position_pointer_test() ->
    true.

song_select_test() ->
    true.

tune_request_test() ->
    true.

end_of_sys_ex_test() ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Real-Time Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rt_clock_test() ->
    Msg1 = {midi, {realtime, clock}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

rt_start_test() ->
    Msg1 = {midi, {realtime, start}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

rt_continue_test() ->
    Msg1 = {midi, {realtime, continue}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

rt_stop_test() ->
    Msg1 = {midi, {realtime, stop}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

rt_active_sensing_test() ->
    Msg1 = {midi, {realtime, active_sensing}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

rt_reset_test() ->
    Msg1 = {midi, {realtime, reset}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System Exclusive Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sys_ex_test() ->
    Msg1 = {midi, {sys_ex, 123}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Unexpected Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

non_midi_test() ->
    Msg1 = {cmd, "do a thing"}, 
    Result1 = roundtrip(Msg1),
    ?assert(?ERR_NON_MIDI =:= Result1),
    Msg2 = {midi, {something, {crazy, "DATA"}}},
    Result2 = roundtrip(Msg2),
    ?assert(?ERR_MIDI_UNSUP =:= Result2).

%%% Utility functions

roundtrip(Input) ->
    io:format("~nInput message: ~p~n", [Input]),
    Bin = midibin:encode(Input),
    io:format("MIDI binary: ~p~n", [Bin]),
    Output = midibin:decode(Bin),
    io:format("Output message: ~p~n", [Output]),
    Output.
