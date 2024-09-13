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
    true.

poly_aftertouch_test() ->
    true.

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
    true.

rt_start_test() ->
    true.

rt_continue_test() ->
    true.

rt_stop_test() ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System Exclusive Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sys_ex_test() ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Unexpected Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

non_midi_test() ->
    Msg1 = {cmd, "do a thing"}, 
    Result1 = roundtrip(Msg1),
    ?assert({unknown, ?ERR_NON_MIDI} =:= Result1),
    Msg2 = {midi, {something, {crazy, "DATA"}}},
    Result2 = roundtrip(Msg2),
    ?assert({unknown, ?ERR_MIDI_UNSUP} =:= Result2).

%%% Utility functions

roundtrip(Input) ->
    io:format("~nInput message: ~p~n", [Input]),
    Bin = midibin:encode(Input),
    io:format("MIDI binary: ~p~n", [Bin]),
    Output = midibin:decode(Bin),
    io:format("Output message: ~p~n", [Output]),
    Output.
