-module(midibin_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/midi_msg.hrl").

%%% Round-trips on the canonical vocabulary: record -> encode -> decode -> record.
%%% Helper asserts encode succeeds and returns decode's result for comparison.

roundtrip(Msg) ->
    {ok, Bin} = midibin:encode(Msg),
    midibin:decode(Bin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Channel-voice messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

note_off_test() ->
    M = #note_off{channel = 11, pitch = 60, velocity = 32},
    ?assertEqual({ok, M}, roundtrip(M)).

note_on_test() ->
    M = #note_on{channel = 11, pitch = 60, velocity = 32},
    ?assertEqual({ok, M}, roundtrip(M)).

%% Velocity 0 must be preserved as note_on (NOT folded to note_off) — C8/R6.
note_on_velocity_zero_preserved_test() ->
    M = #note_on{channel = 1, pitch = 64, velocity = 0},
    ?assertEqual({ok, M}, roundtrip(M)),
    {ok, Bin} = midibin:encode(M),
    ?assertMatch({ok, #note_on{velocity = 0}}, midibin:decode(Bin)).

poly_aftertouch_test() ->
    M = #poly_aftertouch{channel = 11, pitch = 48, pressure = 64},
    ?assertEqual({ok, M}, roundtrip(M)).

program_change_test() ->
    M = #program_change{channel = 8, program = 64},
    ?assertEqual({ok, M}, roundtrip(M)).

channel_aftertouch_test() ->
    M = #channel_aftertouch{channel = 11, pressure = 32},
    ?assertEqual({ok, M}, roundtrip(M)).

pitch_bend_test() ->
    M = #pitch_bend{channel = 11, value = 16000},
    ?assertEqual({ok, M}, roundtrip(M)).

pitch_bend_extremes_test() ->
    [ ?assertEqual({ok, M}, roundtrip(M))
      || V <- [0, 8192, 16383],
         M <- [#pitch_bend{channel = 1, value = V}] ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Channel-mode messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

channel_mode_roundtrip_test() ->
    Modes = [all_sound_off, reset_all_controllers, local_control_off,
             local_control_on, all_notes_off, omni_mode_off, omni_mode_on,
             poly_mode_on],
    [ ?assertEqual({ok, M}, roundtrip(M))
      || Mode <- Modes, M <- [#channel_mode{channel = 3, mode = Mode}] ].

%% mono_mode_on carries the channel-count byte (was not_implemented before).
mono_mode_on_test() ->
    M = #channel_mode{channel = 5, mode = mono_mode_on, value = 4},
    ?assertEqual({ok, M}, roundtrip(M)),
    M0 = #channel_mode{channel = 5, mode = mono_mode_on, value = 0},
    ?assertEqual({ok, M0}, roundtrip(M0)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Control-change & the C5 boundary %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cc_test() ->
    M = #control_change{channel = 2, control = 64, value = 127},
    ?assertEqual({ok, M}, roundtrip(M)).

%% A controller in 120-127 carrying a NON-canonical value is decoded faithfully
%% as a control_change (documented C5 policy), not silently as a mode.
channel_mode_odd_value_is_cc_test() ->
    M = #control_change{channel = 1, control = 123, value = 64},
    ?assertEqual({ok, M}, roundtrip(M)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System-common messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mtc_quarter_frame_test() ->
    M = #mtc_quarter_frame{message_type = 3, value = 7},
    ?assertEqual({ok, M}, roundtrip(M)).

song_position_test() ->
    M = #song_position{position = 9000},
    ?assertEqual({ok, M}, roundtrip(M)).

song_select_test() ->
    M = #song_select{song = 6},
    ?assertEqual({ok, M}, roundtrip(M)).

tune_request_test() ->
    M = #tune_request{},
    ?assertEqual({ok, M}, roundtrip(M)).

end_of_exclusive_test() ->
    M = #end_of_exclusive{},
    ?assertEqual({ok, M}, roundtrip(M)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Real-time messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

realtime_roundtrip_test() ->
    [ ?assertEqual({ok, M}, roundtrip(M))
      || T <- [clock, start, continue, stop, active_sensing, reset],
         M <- [#realtime{type = T}] ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System-exclusive messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The headline fix (#8): arbitrary-length SysEx must round-trip.
sysex_multibyte_test() ->
    Data = << <<(X rem 128)>> || X <- lists:seq(1, 300) >>,  % 300 data bytes, all 0..127
    M = #sysex{data = Data},
    ?assertEqual({ok, M}, roundtrip(M)),
    {ok, Bin} = midibin:encode(M),
    ?assertEqual(16#F0, binary:first(Bin)),
    ?assertEqual(16#F7, binary:last(Bin)),
    ?assertEqual(byte_size(Data) + 2, byte_size(Bin)).

sysex_empty_payload_test() ->
    M = #sysex{data = <<>>},
    ?assertEqual({ok, M}, roundtrip(M)).

sysex_truncated_is_unknown_test() ->
    %% F0 with no terminating F7 is malformed.
    ?assertMatch({error, {unknown, _}}, midibin:decode(<<16#F0, 1, 2, 3>>)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Batch (list in / list out) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

batch_roundtrip_test() ->
    Msgs = [#realtime{type = clock},
            #realtime{type = start},
            #note_on{channel = 1, pitch = 60, velocity = 100},
            #realtime{type = stop}],
    Bins = midibin:encode_batch(Msgs),
    ?assert(lists:all(fun is_binary/1, Bins)),
    ?assertEqual([{ok, M} || M <- Msgs], midibin:decode_batch(Bins)).

batch_empty_test() ->
    ?assertEqual([], midibin:encode_batch([])),
    ?assertEqual([], midibin:decode_batch([])).

%% Short-circuit: good binaries, then a single trailing {error,_}, then stop (R9).
%% The rejected element is a well-typed message() the wire codec can't encode (a
%% meta event is file-only), which is the realistic short-circuit trigger.
batch_short_circuits_on_error_test() ->
    Msgs = [#realtime{type = clock},
            #realtime{type = start},
            #meta_set_tempo{usec_per_quarter = 500000},
            #realtime{type = stop}],
    Result = midibin:encode_batch(Msgs),
    [B1, B2 | Tail] = Result,
    ?assert(is_binary(B1) andalso is_binary(B2)),
    ?assertMatch([{error, {unsupported, _}}], Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Error contract %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_unknown_test() ->
    %% Unrecognised wire bytes decode to {error,{unknown,Bin}} (DESIGN §6) —
    %% e.g. the undefined system-common status F4, or a lone data byte.
    ?assertMatch({error, {unknown, _}}, midibin:decode(<<16#F4>>)),
    ?assertMatch({error, {unknown, _}}, midibin:decode(<<16#F5>>)),
    ?assertMatch({error, {unknown, _}}, midibin:decode(<<0:8>>)).

encode_meta_is_unsupported_test() ->
    %% Meta events are file-only; the wire codec rejects them with a clear reason.
    ?assertMatch({error, {unsupported, _}},
                 midibin:encode(#meta_set_tempo{usec_per_quarter = 500000})).

encode_non_message_test() ->
    ?assertEqual({error, non_midi}, midibin:encode("not a record")),
    ?assertMatch({error, {unsupported, _}}, midibin:encode({some, tuple})).

encode_out_of_range_test() ->
    %% Out-of-range fields do not silently truncate; they fail.
    ?assertMatch({error, _}, midibin:encode(#note_on{channel = 99, pitch = 60, velocity = 1})),
    ?assertMatch({error, _}, midibin:encode(#note_on{channel = 1, pitch = 200, velocity = 1})).
