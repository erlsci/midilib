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

program_change_test() ->
    Msg1 = {midi, {program_change, [{channel, 8},
                                    {program, 64}]}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

pitch_bend_test() ->
    Msg1 = {midi, {pitch_bend, [{channel, 11},
                                {value, 16000}]}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Channel Mode Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_sound_off_test() ->
    Msg1 = {midi, all_sound_off},
    Result1 = roundtrip(Msg1),
    Msg11 = {midi, {mode, [{channel, 1},
                           {control, all_sound_off}]}},
    ?assert(Result1 =:= Msg11),
    Msg2 = {midi, {mode, [{channel, 3},
                          {control, all_sound_off}]}},
    Result2 = roundtrip(Msg2),
    ?assert(Result2 =:= Msg2).

reset_all_test() ->
    Msg1 = {midi, reset_all},
    Result1 = roundtrip(Msg1),
    Msg11 = {midi, {mode, [{channel, 1},
                           {control, reset_all}]}},
    ?assert(Result1 =:= Msg11),
    Msg2 = {midi, {mode, [{channel, 3},
                          {control, reset_all}]}},
    Result2 = roundtrip(Msg2),
    ?assert(Result2 =:= Msg2).

local_control_off_test() ->
    Msg1 = {midi, local_control_off},
    Result1 = roundtrip(Msg1),
    Msg11 = {midi, {mode, [{channel, 1},
                           {control, local_control_off}]}},
    ?assert(Result1 =:= Msg11),
    Msg2 = {midi, {mode, [{channel, 3},
                          {control, local_control_off}]}},
    Result2 = roundtrip(Msg2),
    ?assert(Result2 =:= Msg2).

local_control_on_test() ->
    Msg1 = {midi, local_control_on},
    Result1 = roundtrip(Msg1),
    Msg11 = {midi, {mode, [{channel, 1},
                           {control, local_control_on}]}},
    ?assert(Result1 =:= Msg11),
    Msg2 = {midi, {mode, [{channel, 3},
                          {control, local_control_on}]}},
    Result2 = roundtrip(Msg2),
    ?assert(Result2 =:= Msg2).

all_notes_off_test() ->
    Msg1 = {midi, all_notes_off},
    Result1 = roundtrip(Msg1),
    Msg11 = {midi, {mode, [{channel, 1},
                           {control, all_notes_off}]}},
    ?assert(Result1 =:= Msg11),
    Msg2 = {midi, {mode, [{channel, 3},
                          {control, all_notes_off}]}},
    Result2 = roundtrip(Msg2),
    ?assert(Result2 =:= Msg2).

omni_mode_off_test() ->
    Msg1 = {midi, omni_mode_off},
    Result1 = roundtrip(Msg1),
    Msg11 = {midi, {mode, [{channel, 1},
                           {control, omni_mode_off}]}},
    ?assert(Result1 =:= Msg11),
    Msg2 = {midi, {mode, [{channel, 3},
                          {control, omni_mode_off}]}},
    Result2 = roundtrip(Msg2),
    ?assert(Result2 =:= Msg2).

omni_mode_on_test() ->
    Msg1 = {midi, omni_mode_on},
    Result1 = roundtrip(Msg1),
    Msg11 = {midi, {mode, [{channel, 1},
                           {control, omni_mode_on}]}},
    ?assert(Result1 =:= Msg11),
    Msg2 = {midi, {mode, [{channel, 3},
                          {control, omni_mode_on}]}},
    Result2 = roundtrip(Msg2),
    ?assert(Result2 =:= Msg2).

%mono_mode_on_test() ->
%    true.

poly_mode_on_test() ->
    Msg1 = {midi, poly_mode_on},
    Result1 = roundtrip(Msg1),
    Msg11 = {midi, {mode, [{channel, 1},
                           {control, poly_mode_on}]}},
    ?assert(Result1 =:= Msg11),
    Msg2 = {midi, {mode, [{channel, 3},
                          {control, poly_mode_on}]}},
    Result2 = roundtrip(Msg2),
    ?assert(Result2 =:= Msg2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% ENCODE: Control Change Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cc_test() ->
    Msg1 = {midi, {cc, [{channel, 2},
                        {control, 64},
                        {value, 127}]}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System Common Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

time_code_quarter_frame_test() ->
    Msg1 = {midi, {time_code_quarter_frame, 3, 7}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

song_position_pointer_test() ->
    Msg1 = {midi, {song_position_pointer, 9000}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

song_select_test() ->
    Msg1 = {midi, {song_select, 6}},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

tune_request_test() ->
    Msg1 = {midi, tune_request},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

end_of_sys_ex_test() ->
    Msg1 = {midi, end_of_sys_ex},
    Result1 = roundtrip(Msg1),
    ?assert(Result1 =:= Msg1).

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
