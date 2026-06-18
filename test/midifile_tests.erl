-module(midifile_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/midi_msg.hrl").

%%% The reader's evidence. Fixtures are inline binaries so the exact triggering
%%% bytes are visible in review; each is written to a temp file and read back
%%% through the real midifile:read/1 (so file I/O and the #15 error path are
%%% exercised too). The high-value rows are the Blocker (#1/S2 multi-byte SysEx)
%%% and the read-side correctness fixes (#4/S4, S7, M2, #7/S8, #13/C7).

%%% --------------------------------------------------------------------------
%%% Fixture helpers
%%% --------------------------------------------------------------------------

%% Write Bin to a temp .mid and read it back through the public entry point.
read_bytes(Bin) ->
    Dir = tmp_dir(),
    Name = "midifile_test_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".mid",
    Path = filename:join(Dir, Name),
    ok = file:write_file(Path, Bin),
    try midifile:read(Path)
    after file:delete(Path)
    end.

tmp_dir() ->
    case os:getenv("TMPDIR") of
        false -> "/tmp";
        ""    -> "/tmp";
        Dir   -> Dir
    end.

%% MThd with an explicit 2-byte division word.
mthd(Format, NumTracks, <<_:16>> = Division) ->
    <<"MThd", 6:32, Format:16, NumTracks:16, Division/binary>>.

%% MTrk wrapping a body of already-assembled event bytes.
mtrk(Body) ->
    <<"MTrk", (byte_size(Body)):32, Body/binary>>.

%% A standard 480-ppqn division word.
ppqn_480() -> <<16#01, 16#E0>>.

%% delta 0, FF 2F 00 — end of track.
eot() -> <<0, 16#FF, 16#2F, 0>>.

%% A one-track format-0 file from a track body, ppqn 480.
smf0(Body) ->
    <<(mthd(0, 1, ppqn_480()))/binary, (mtrk(Body))/binary>>.

%% Read a one-track format-0 file and return that track's event list.
events_of(Body) ->
    {ok, #seq{tracks = [#track{events = Events}]}} = read_bytes(smf0(Body)),
    Events.

%%% --------------------------------------------------------------------------
%%% Header / containers / division
%%% --------------------------------------------------------------------------

%% read/1 :: -> {ok,#seq{}} | {error, midierrs:reason()} — no untagged tuple.
read_open_failure_test() ->
    Path = filename:join(tmp_dir(), "does_not_exist_" ++
                             integer_to_list(erlang:unique_integer([positive]))),
    ?assertEqual({error, {open, Path, enoent}}, midifile:read(Path)).

%% M2: a file that opens but has no MThd is a value, not a crash/scan.
not_midi_file_test() ->
    ?assertMatch({error, {not_midi_file, _}}, read_bytes(<<"RIFF....not a midi">>)).

ppqn_division_test() ->
    {ok, Seq} = read_bytes(smf0(eot())),
    ?assertEqual({ppqn, 480}, Seq#seq.division).

%% SMPTE: high byte 0xE7 = -25 (two's complement) => 25 fps; low byte 0x28 = 40
%% ticks/frame.
smpte_division_test() ->
    Bin = <<(mthd(0, 1, <<16#E7, 16#28>>))/binary, (mtrk(eot()))/binary>>,
    {ok, Seq} = read_bytes(Bin),
    ?assertEqual({smpte, 25, 40}, Seq#seq.division).

%% Format 0 and format 1 both read into a uniform track list (no conductor
%% split): format 1's "conductor" is simply hd(tracks).
format0_read_test() ->
    {ok, Seq} = read_bytes(smf0(eot())),
    ?assertEqual(0, Seq#seq.format),
    ?assertMatch([#track{}], Seq#seq.tracks).

format1_read_test() ->
    Track = mtrk(eot()),
    Bin = <<(mthd(1, 2, ppqn_480()))/binary, Track/binary, Track/binary>>,
    {ok, Seq} = read_bytes(Bin),
    ?assertEqual(1, Seq#seq.format),
    ?assertMatch([#track{}, #track{}], Seq#seq.tracks).

%%% --------------------------------------------------------------------------
%%% #1 / S2 — multi-byte SysEx reads without desync (the Blocker)
%%% --------------------------------------------------------------------------

%% A 300-byte SysEx payload (299 data bytes + trailing F7); its VLQ length is
%% two bytes (300 = 0x82 0x2C). A NOTE-ON follows it: if the F0-byte accounting
%% is wrong the parser desyncs and the note-on is misread, so a correct read of
%% the following note-on is the proof the Blocker is fixed.
sysex_multibyte_read_test() ->
    Data = << <<(X rem 128)>> || X <- lists:seq(1, 299) >>,  % 299 bytes, all 0..127
    Payload = <<Data/binary, 16#F7>>,                        % + trailing F7 => 300
    300 = byte_size(Payload),
    Body = <<0, 16#F0, 16#82, 16#2C, Payload/binary,         % delta 0, sysex
             0, 16#90, 60, 100,                              % delta 0, note-on
             (eot())/binary>>,
    [SysexEv, NoteEv, EotEv] = events_of(Body),
    ?assertEqual(#event{delta = 0, message = #sysex{data = Data}}, SysexEv),
    ?assertEqual(#event{delta = 0,
                        message = #note_on{channel = 1, pitch = 60, velocity = 100}},
                 NoteEv),
    ?assertEqual(#meta_end_of_track{}, EotEv#event.message).

%% An empty-payload SysEx (length 0) reads to #sysex{data = <<>>}.
sysex_empty_payload_read_test() ->
    [Ev | _] = events_of(<<0, 16#F0, 0, (eot())/binary>>),
    ?assertEqual(#sysex{data = <<>>}, Ev#event.message).

%% An F7-led event (a SysEx "escape"/continuation) shares the VLQ framing; its
%% raw bytes (no trailing F7 here) are kept verbatim.
sysex_f7_escape_read_test() ->
    [Ev | _] = events_of(<<0, 16#F7, 3, 16#43, 16#12, 16#00, (eot())/binary>>),
    ?assertEqual(#sysex{data = <<16#43, 16#12, 16#00>>}, Ev#event.message).

%%% --------------------------------------------------------------------------
%%% #4 / S4 — sequencer-specific is distinct from track-name
%%% --------------------------------------------------------------------------

track_name_read_test() ->
    [Ev | _] = events_of(<<0, 16#FF, 16#03, 4, "Drum", (eot())/binary>>),
    ?assertEqual(#meta_track_name{name = <<"Drum">>}, Ev#event.message).

sequencer_specific_read_test() ->
    [Ev | _] = events_of(<<0, 16#FF, 16#7F, 3, 16#41, 16#01, 16#02, (eot())/binary>>),
    ?assertEqual(#meta_sequencer_specific{data = <<16#41, 16#01, 16#02>>},
                 Ev#event.message).

%% The two must not collapse to the same record (the #4/S4 mis-tag).
sequencer_specific_distinct_from_track_name_test() ->
    [Name | _] = events_of(<<0, 16#FF, 16#03, 2, "Hi", (eot())/binary>>),
    [Spec | _] = events_of(<<0, 16#FF, 16#7F, 2, 1, 2, (eot())/binary>>),
    ?assertNotEqual(element(1, Name#event.message), element(1, Spec#event.message)).

%%% --------------------------------------------------------------------------
%%% S7 — unterminated VLQ is an error value, not a silent number
%%% --------------------------------------------------------------------------

bad_vlq_read_test() ->
    %% Four delta-time bytes all with the continuation bit set: malformed.
    ?assertMatch({error, {bad_vlq, _}},
                 read_bytes(smf0(<<16#FF, 16#FF, 16#FF, 16#FF, 0>>))).

%%% --------------------------------------------------------------------------
%%% #7 / S8 / C8 — Note-On velocity 0 read faithfully (no fold, no vel 64)
%%% --------------------------------------------------------------------------

note_on_vel0_faithful_read_test() ->
    [Ev | _] = events_of(<<0, 16#90, 64, 0, (eot())/binary>>),
    ?assertEqual(#note_on{channel = 1, pitch = 64, velocity = 0}, Ev#event.message).

%%% --------------------------------------------------------------------------
%%% #13 / C7 — channels are 1-based in the read output
%%% --------------------------------------------------------------------------

channel_base_read_test() ->
    [Lo | _] = events_of(<<0, 16#90, 60, 1, (eot())/binary>>),   % nibble 0  -> ch 1
    [Hi | _] = events_of(<<0, 16#9F, 60, 1, (eot())/binary>>),   % nibble 15 -> ch 16
    ?assertEqual(1, (Lo#event.message)#note_on.channel),
    ?assertEqual(16, (Hi#event.message)#note_on.channel).

%%% --------------------------------------------------------------------------
%%% #18 — running status threaded; a status-elided run reads correctly
%%% --------------------------------------------------------------------------

running_status_read_test() ->
    %% One 0x90 status byte, then two events that elide it (data bytes only).
    Body = <<0, 16#90, 60, 100,        % note-on, explicit status
             0, 62, 100,               % running status
             0, 64, 100,               % running status
             (eot())/binary>>,
    [E1, E2, E3, _Eot] = events_of(Body),
    ?assertEqual(#note_on{channel = 1, pitch = 60, velocity = 100}, E1#event.message),
    ?assertEqual(#note_on{channel = 1, pitch = 62, velocity = 100}, E2#event.message),
    ?assertEqual(#note_on{channel = 1, pitch = 64, velocity = 100}, E3#event.message).

%% Running status is reset at the start of each track (not carried across).
running_status_reset_per_track_test() ->
    T1 = mtrk(<<0, 16#90, 60, 100, (eot())/binary>>),
    %% Track 2 opens with explicit status too; both tracks decode independently.
    T2 = mtrk(<<0, 16#80, 60, 64, (eot())/binary>>),
    Bin = <<(mthd(1, 2, ppqn_480()))/binary, T1/binary, T2/binary>>,
    {ok, #seq{tracks = [#track{events = [Ev1 | _]}, #track{events = [Ev2 | _]}]}} =
        read_bytes(Bin),
    ?assertEqual(#note_on{channel = 1, pitch = 60, velocity = 100}, Ev1#event.message),
    ?assertEqual(#note_off{channel = 1, pitch = 60, velocity = 64}, Ev2#event.message).

%%% --------------------------------------------------------------------------
%%% Row 17 — one fixture per event family reads to the expected record
%%% --------------------------------------------------------------------------

channel_voice_families_test() ->
    Cases =
        [{<<16#80, 60, 64>>,  #note_off{channel = 1, pitch = 60, velocity = 64}},
         {<<16#90, 60, 100>>, #note_on{channel = 1, pitch = 60, velocity = 100}},
         {<<16#A0, 48, 90>>,  #poly_aftertouch{channel = 1, pitch = 48, pressure = 90}},
         {<<16#B0, 7, 110>>,  #control_change{channel = 1, control = 7, value = 110}},
         {<<16#C0, 42>>,      #program_change{channel = 1, program = 42}},
         {<<16#D0, 70>>,      #channel_aftertouch{channel = 1, pressure = 70}},
         {<<16#E0, 0, 64>>,   #pitch_bend{channel = 1, value = 8192}}],
    [begin
         [Ev | _] = events_of(<<0, Bytes/binary, (eot())/binary>>),
         ?assertEqual(Expected, Ev#event.message)
     end || {Bytes, Expected} <- Cases].

channel_mode_family_test() ->
    [Ev | _] = events_of(<<0, 16#B0, 123, 0, (eot())/binary>>),
    ?assertEqual(#channel_mode{channel = 1, mode = all_notes_off}, Ev#event.message).

meta_families_test() ->
    Cases =
        [{<<16#FF, 16#00, 2, 0, 5>>,             #meta_sequence_number{value = 5}},
         {<<16#FF, 16#01, 2, "hi">>,             #meta_text{text = <<"hi">>}},
         {<<16#FF, 16#02, 3, "(c)">>,            #meta_copyright{text = <<"(c)">>}},
         {<<16#FF, 16#04, 5, "Piano">>,          #meta_instrument_name{name = <<"Piano">>}},
         {<<16#FF, 16#05, 2, "la">>,             #meta_lyric{text = <<"la">>}},
         {<<16#FF, 16#06, 1, "M">>,              #meta_marker{text = <<"M">>}},
         {<<16#FF, 16#07, 1, "C">>,              #meta_cue_point{text = <<"C">>}},
         {<<16#FF, 16#20, 1, 3>>,                #meta_channel_prefix{channel = 4}},
         {<<16#FF, 16#51, 3, 16#07, 16#A1, 16#20>>,
          #meta_set_tempo{usec_per_quarter = 500000}},
         {<<16#FF, 16#54, 5, 1, 2, 3, 4, 5>>,
          #meta_smpte_offset{hour = 1, minute = 2, second = 3,
                             frame = 4, sub_frame = 5}},
         {<<16#FF, 16#58, 4, 6, 3, 24, 8>>,
          #meta_time_signature{numerator = 6, denominator = 8,
                               clocks_per_click = 24, notated_32nd_per_quarter = 8}},
         {<<16#FF, 16#59, 2, 2, 0>>,             #meta_key_signature{key = 2, mode = major}},
         {<<16#FF, 16#59, 2, 16#FD, 1>>,         #meta_key_signature{key = -3, mode = minor}},
         {<<16#FF, 16#60, 2, 9, 9>>,             #meta_unknown{type = 16#60, data = <<9, 9>>}}],
    [begin
         [Ev | _] = events_of(<<0, Bytes/binary, (eot())/binary>>),
         ?assertEqual(Expected, Ev#event.message)
     end || {Bytes, Expected} <- Cases].

end_of_track_family_test() ->
    [Ev] = events_of(eot()),
    ?assertEqual(#meta_end_of_track{}, Ev#event.message).

%% Delta-times are read as the VLQ tick count (here a 2-byte VLQ, 0x81 0x00 = 128).
delta_time_read_test() ->
    [Ev | _] = events_of(<<16#81, 16#00, 16#90, 60, 100, (eot())/binary>>),
    ?assertEqual(128, Ev#event.delta).

%%% --------------------------------------------------------------------------
%%% Canonical-vocabulary contract (M1) + write stub
%%% --------------------------------------------------------------------------

%% A small multi-event track round-trips through the canonical records only.
canonical_records_only_test() ->
    Body = <<0, 16#FF, 16#03, 4, "Lead",
             0, 16#C0, 5,
             0, 16#90, 60, 100,
             (eot())/binary>>,
    ?assertMatch([#event{message = #meta_track_name{}},
                  #event{message = #program_change{}},
                  #event{message = #note_on{}},
                  #event{message = #meta_end_of_track{}}],
                 events_of(Body)).

write_is_slice_b_stub_test() ->
    Seq = #seq{format = 0, division = {ppqn, 480}, tracks = []},
    ?assertEqual({error, not_implemented}, midifile:write(Seq, "/tmp/unused.mid")).
