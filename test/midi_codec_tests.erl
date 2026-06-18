-module(midi_codec_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/midi_msg.hrl").

%%% Unit tests for the pure message<->bytes core. The headline contracts
%%% (ledger row 2): both directions round-trip, channels are 1-based, the C5
%%% channel-mode policy holds, and Note-On velocity 0 is never folded.

%% Round-trip a message through encode_message/1 then decode_message/2.
message_rt(Msg) ->
    {ok, {Status, Data}} = midi_codec:encode_message(Msg),
    midi_codec:decode_message(Status, Data).

%% Round-trip a meta record through encode_meta/1 then decode_meta/2.
meta_rt(Meta) ->
    {ok, {Type, Payload}} = midi_codec:encode_meta(Meta),
    midi_codec:decode_meta(Type, Payload).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Channel-voice round-trips (±1 chan) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

channel_voice_roundtrip_test_() ->
    Msgs = [#note_off{channel = 1, pitch = 60, velocity = 64},
            #note_on{channel = 16, pitch = 127, velocity = 1},
            #poly_aftertouch{channel = 9, pitch = 48, pressure = 100},
            #control_change{channel = 2, control = 7, value = 110},
            #program_change{channel = 5, program = 42},
            #channel_aftertouch{channel = 11, pressure = 70},
            #pitch_bend{channel = 1, value = 0},
            #pitch_bend{channel = 1, value = 8192},
            #pitch_bend{channel = 1, value = 16383}],
    [?_assertEqual({ok, M}, message_rt(M)) || M <- Msgs].

%% Channel 1 is the low wire nibble (0), channel 16 is nibble 15 — 1-based.
channel_base_1_based_test() ->
    ?assertEqual({ok, #note_on{channel = 1, pitch = 60, velocity = 1}},
                 midi_codec:decode_message(16#90, <<60, 1>>)),
    ?assertEqual({ok, #note_on{channel = 16, pitch = 60, velocity = 1}},
                 midi_codec:decode_message(16#9F, <<60, 1>>)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Channel-mode (C5 policy) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

channel_mode_roundtrip_test_() ->
    Modes = [all_sound_off, reset_all_controllers, local_control_off,
             local_control_on, all_notes_off, omni_mode_off, omni_mode_on,
             poly_mode_on],
    [?_assertEqual({ok, #channel_mode{channel = 3, mode = Mode}},
                   message_rt(#channel_mode{channel = 3, mode = Mode}))
     || Mode <- Modes].

mono_mode_on_carries_count_test() ->
    M = #channel_mode{channel = 5, mode = mono_mode_on, value = 4},
    ?assertEqual({ok, M}, message_rt(M)),
    %% Decoded straight from the wire bytes: controller 126, value 4.
    ?assertEqual({ok, M}, midi_codec:decode_message(16#B4, <<126, 4>>)).

%% A controller in 120-127 with a NON-canonical value is a faithful
%% control_change, not silently a channel-mode message (the C5 boundary).
controller_120_127_odd_value_is_cc_test() ->
    M = #control_change{channel = 1, control = 123, value = 64},
    ?assertEqual({ok, M}, message_rt(M)),
    ?assertEqual({ok, M}, midi_codec:decode_message(16#B0, <<123, 64>>)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% No velocity-0 fold (C8/R6) %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

note_on_velocity_zero_not_folded_test() ->
    %% 0x9n note N vel 0 stays note_on vel 0 — never a note_off, never vel 64.
    ?assertEqual({ok, #note_on{channel = 1, pitch = 64, velocity = 0}},
                 midi_codec:decode_message(16#90, <<64, 0>>)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System messages %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

system_roundtrip_test_() ->
    Msgs = [#mtc_quarter_frame{message_type = 3, value = 7},
            #song_position{position = 9000},
            #song_select{song = 6},
            #tune_request{},
            #end_of_exclusive{},
            #realtime{type = clock},
            #realtime{type = start},
            #realtime{type = continue},
            #realtime{type = stop},
            #realtime{type = active_sensing},
            #realtime{type = reset}],
    [?_assertEqual({ok, M}, message_rt(M)) || M <- Msgs].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% data_length/1 table %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_length_test() ->
    ?assertEqual(2, midi_codec:data_length(16#90)),  % note on
    ?assertEqual(2, midi_codec:data_length(16#B5)),  % control change
    ?assertEqual(1, midi_codec:data_length(16#C0)),  % program change
    ?assertEqual(1, midi_codec:data_length(16#D0)),  % channel pressure
    ?assertEqual(2, midi_codec:data_length(16#E0)),  % pitch bend
    ?assertEqual(1, midi_codec:data_length(16#F1)),  % mtc quarter frame
    ?assertEqual(2, midi_codec:data_length(16#F2)),  % song position
    ?assertEqual(1, midi_codec:data_length(16#F3)),  % song select
    ?assertEqual(0, midi_codec:data_length(16#F6)),  % tune request
    ?assertEqual(variable, midi_codec:data_length(16#F0)),
    ?assertEqual(variable, midi_codec:data_length(16#F7)),
    ?assertEqual(0, midi_codec:data_length(16#F8)),  % clock
    ?assertEqual(0, midi_codec:data_length(16#FF)).  % (reset, on the wire)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Unknown / fallback %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_unknown_status_is_value_test() ->
    %% Undefined system-common status F4 with no body: reconstructed bytes.
    ?assertEqual({error, {unknown, <<16#F4>>}},
                 midi_codec:decode_message(16#F4, <<>>)),
    %% A wrong data length for a real status also reconstructs the bytes.
    ?assertMatch({error, {unknown, <<16#90, 60>>}},
                 midi_codec:decode_message(16#90, <<60>>)).

encode_non_message_test() ->
    ?assertEqual({error, non_midi}, midi_codec:encode_message("nope")),
    ?assertMatch({error, {unsupported, _}}, midi_codec:encode_message({weird, tuple})),
    %% Meta records are not wire bodies; they go through encode_meta/1.
    ?assertMatch({error, {unsupported, _}},
                 midi_codec:encode_message(#meta_set_tempo{usec_per_quarter = 500000})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Meta value conversions %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meta_roundtrip_test_() ->
    Metas = [#meta_sequence_number{value = 5},
             #meta_text{text = <<"a note">>},
             #meta_copyright{text = <<"(c) 2026">>},
             #meta_track_name{name = <<"Lead">>},
             #meta_instrument_name{name = <<"Piano">>},
             #meta_lyric{text = <<"la">>},
             #meta_marker{text = <<"Verse">>},
             #meta_cue_point{text = <<"enter">>},
             #meta_channel_prefix{channel = 4},
             #meta_set_tempo{usec_per_quarter = 500000},
             #meta_smpte_offset{hour = 1, minute = 2, second = 3,
                                frame = 4, sub_frame = 5},
             #meta_time_signature{numerator = 6, denominator = 8,
                                  clocks_per_click = 24,
                                  notated_32nd_per_quarter = 8},
             #meta_key_signature{key = 2, mode = major},
             #meta_key_signature{key = -3, mode = minor},
             #meta_sequencer_specific{data = <<1, 2, 3>>},
             #meta_end_of_track{},
             #meta_unknown{type = 16#60, data = <<9, 9>>}],
    [?_assertEqual({ok, M}, meta_rt(M)) || M <- Metas].

%% tempo encodes to a 24-bit µs/qn payload; time-sig denominator is 2^dd.
meta_byte_layout_test() ->
    ?assertEqual({ok, {16#51, <<16#07, 16#A1, 16#20>>}},
                 midi_codec:encode_meta(#meta_set_tempo{usec_per_quarter = 500000})),
    ?assertEqual({ok, {16#58, <<6, 3, 24, 8>>}},
                 midi_codec:encode_meta(
                   #meta_time_signature{numerator = 6, denominator = 8,
                                        clocks_per_click = 24,
                                        notated_32nd_per_quarter = 8})).

%% A non-power-of-two denominator is a structured error, not a silent rounding.
meta_time_sig_bad_denominator_test() ->
    ?assertEqual({error, {bad_value, denominator, 3}},
                 midi_codec:encode_meta(
                   #meta_time_signature{numerator = 4, denominator = 3,
                                        clocks_per_click = 24,
                                        notated_32nd_per_quarter = 8})).

%% A modelled meta type with an unexpected payload length round-trips verbatim
%% as #meta_unknown{} (lossless), rather than being silently coerced.
meta_unknown_preserves_unmodelled_type_test() ->
    ?assertEqual({ok, #meta_unknown{type = 16#7A, data = <<1, 2, 3>>}},
                 midi_codec:decode_meta(16#7A, <<1, 2, 3>>)).
