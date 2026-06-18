%%% ===========================================================================
%%% midifile — Standard MIDI File (SMF) codec.
%%%
%%% The FILE framer: chunk structure (MThd/MTrk), the variable-length-quantity
%%% codec, delta-times, and running-status elision/expansion. Message bodies are
%%% mapped by midi_codec (the shared, framing-free core), so the file and wire
%%% codecs can never disagree on how a message is laid out.
%%%
%%% read/1 reads the whole file into a binary and parses it functionally — no
%%% per-byte file:pread scan (audit #22), no process-dictionary running-status
%%% state (audit #18): the last status byte is threaded as an argument and reset
%%% at the start of each track. Output is the canonical vocabulary
%%% (include/midi_msg.hrl): #seq{format, division, tracks} with a uniform track
%%% list (no conductor-track split — format 1's conductor is hd(tracks)), typed
%%% division ({ppqn,N} | {smpte,Fps,Tpf}), and 1-based channels (the ±1 lives in
%%% midi_codec, at the file edge only).
%%%
%%% Predictable failures are values (DESIGN §6): a file that opens but has no
%%% MThd is {error, {not_midi_file, Path}}; an open failure is
%%% {error, {open, Path, Reason}}; a malformed VLQ is {error, {bad_vlq, Bytes}}.
%%% Genuinely malformed track bytes (a truncated event, a data byte with no
%%% running status to apply) crash — let it crash (R4); they are not part of the
%%% predictable contract.
%%%
%%% Kept portable: the v0.6.0 support floor is OTP 22-29 (Arc-3 row 12), so the
%%% OTP-27+ -moduledoc/-doc attributes are intentionally not used.
%%% ===========================================================================
-module(midifile).
-export([read/1, write/2]).
-author("Jim Menard, jim@jimmenard.com").

-include("include/midi_msg.hrl").
-include("include/midi.hrl").

%%% ===========================================================================
%%% READ
%%% ===========================================================================
%% Read an SMF from disk into the canonical #seq{}. Whole-file read, then a pure
%% in-memory parse (the parser is exercised directly by the test fixtures).
-spec read(file:name_all()) -> {ok, #seq{}} | {error, midierrs:reason()}.
read(Path) ->
    case file:read_file(Path) of
        {ok, Bin}       -> parse(Bin, Path);
        {error, Reason} -> {error, {open, Path, Reason}}
    end.

%% Parse a complete SMF binary. A valid header is `MThd <len:32> <body>` with
%% len >= 6 and a format in 0..2; anything else is {not_midi_file, Path} (M2) —
%% including a file that opens but does not start with a usable MThd chunk.
-spec parse(binary(), file:name_all()) -> {ok, #seq{}} | {error, midierrs:reason()}.
parse(<<"MThd", Len:32, Body:Len/binary, Rest/binary>>, Path)
  when Len >= 6 ->
    <<Format:16, NumTracks:16, DivHi:8, DivLo:8, _/binary>> = Body,
    case Format =< 2 of
        true ->
            Division = decode_division(DivHi, DivLo),
            case parse_tracks(Rest, NumTracks, []) of
                {ok, Tracks} ->
                    {ok, #seq{format = Format, division = Division, tracks = Tracks}};
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, {not_midi_file, Path}}
    end;
parse(_Bin, Path) ->
    {error, {not_midi_file, Path}}.

%% Division word: high bit set => SMPTE (high byte is the negative frame rate in
%% two's complement, low byte is ticks-per-frame); clear => ticks per quarter
%% note. A zero division (either form) is malformed and crashes (R4).
-spec decode_division(0..255, 0..255) ->
          {ppqn, pos_integer()} | {smpte, pos_integer(), pos_integer()}.
decode_division(Hi, Tpf) when Hi >= 16#80, Tpf >= 1 ->
    {smpte, 256 - Hi, Tpf};
decode_division(Hi, Lo) when Hi < 16#80, ((Hi bsl 8) bor Lo) >= 1 ->
    {ppqn, (Hi bsl 8) bor Lo}.

%% Parse NumTracks MTrk chunks. NumTracks counts MTrk chunks only (RP-001), so
%% an alien (non-MTrk) chunk is skipped by its own declared length and does NOT
%% decrement the count — the SMF spec anticipates alien chunks and says to "treat
%% them as if they weren't there." Bytes/chunks after the last track are ignored.
%% A chunk whose declared length overruns the remaining bytes (truncation), or
%% the header promising more MTrk chunks than the file holds, matches no clause
%% and crashes — genuinely malformed, not spec-legal (R4).
-spec parse_tracks(binary(), non_neg_integer(), [#track{}]) ->
          {ok, [#track{}]} | {error, midierrs:reason()}.
parse_tracks(_Bin, 0, Acc) ->
    {ok, lists:reverse(Acc)};
parse_tracks(<<"MTrk", Len:32, TrackData:Len/binary, Rest/binary>>, N, Acc)
  when N > 0 ->
    case parse_track(TrackData) of
        {ok, Track}        -> parse_tracks(Rest, N - 1, [Track | Acc]);
        {error, _} = Error -> Error
    end;
%% Alien chunk: skip its declared length, keep the track count (FP-12: this must
%% follow the MTrk clause, which also matches this shape and must win).
parse_tracks(<<_Type:4/binary, Len:32, _Skip:Len/binary, Rest/binary>>, N, Acc)
  when N > 0 ->
    parse_tracks(Rest, N, Acc).

-spec parse_track(binary()) -> {ok, #track{}} | {error, midierrs:reason()}.
parse_track(TrackData) ->
    %% Running status is reset (undefined) at the start of every track (#18).
    case parse_events(TrackData, undefined, []) of
        {ok, Events}       -> {ok, #track{events = Events}};
        {error, _} = Error -> Error
    end.

%% The per-track event loop. State is the running-status byte (the last channel
%% status, or `undefined`), threaded as an argument — no process dictionary.
-spec parse_events(binary(), 0..255 | undefined, [#event{}]) ->
          {ok, [#event{}]} | {error, midierrs:reason()}.
parse_events(<<>>, _Running, Acc) ->
    {ok, lists:reverse(Acc)};
parse_events(Bin, Running, Acc) ->
    case read_vlq(Bin) of
        {ok, Delta, Rest} ->
            case parse_event(Rest, Running) of
                {ok, Message, NewRunning, Rest2} ->
                    Event = #event{delta = Delta, message = Message},
                    parse_events(Rest2, NewRunning, [Event | Acc]);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% Parse one event body (the bytes after its delta-time), given the current
%% running status. Returns the message, the new running status, and the rest.
-spec parse_event(binary(), 0..255 | undefined) ->
          {ok, message(), 0..255 | undefined, binary()}
        | {error, midierrs:reason()}.
%% Meta: FF <type> <vlq-len> <payload>. Clears running status.
parse_event(<<?STATUS_META_EVENT, Type:8, Rest/binary>>, _Running) ->
    case read_vlq(Rest) of
        {ok, Len, Rest2} ->
            case Rest2 of
                <<Payload:Len/binary, Rest3/binary>> ->
                    %% decode_meta/2 is total (unmodelled types -> #meta_unknown{}).
                    {ok, Message} = midi_codec:decode_meta(Type, Payload),
                    {ok, Message, undefined, Rest3};
                _ ->
                    error({truncated_meta, Type, Rest2})
            end;
        {error, _} = Error ->
            Error
    end;
%% SysEx: F0 <vlq-len> <payload-including-trailing-F7>. The #1/S2 fix is in the
%% accounting: the F0 byte is consumed by this match, the length VLQ follows,
%% and exactly Len payload bytes are taken — so a multi-byte SysEx never
%% desyncs the parser. The trailing F7 is stripped into #sysex.data. An F7-led
%% event (a SysEx "escape"/continuation) shares the same VLQ framing.
parse_event(<<?STATUS_SYSEX, Rest/binary>>, _Running) ->
    parse_sysex(Rest);
parse_event(<<?STATUS_EOX, Rest/binary>>, _Running) ->
    parse_sysex(Rest);
%% An explicit status byte (high bit set): a channel or system message.
parse_event(<<Status:8, Rest/binary>>, Running) when Status >= 16#80 ->
    read_data(Status, Rest, Running);
%% A data byte (high bit clear) with a running status to apply: the remembered
%% status byte governs, and the data bytes start here.
parse_event(<<First:8, _/binary>> = Bin, Running)
  when First < 16#80, Running =/= undefined ->
    read_data(Running, Bin, Running).

%% Take the data bytes a status consumes (midi_codec:data_length/1) and decode.
%% Status here is always a channel or system byte (F0/F7/FF are handled above),
%% so data_length is never `variable`. Channel messages set running status;
%% real-time messages leave it untouched; system-common clears it.
-spec read_data(0..255, binary(), 0..255 | undefined) ->
          {ok, message(), 0..255 | undefined, binary()}
        | {error, midierrs:reason()}.
read_data(Status, DataAndRest, OldRunning) ->
    case midi_codec:data_length(Status) of
        N when is_integer(N) ->
            case DataAndRest of
                <<Data:N/binary, Rest/binary>> ->
                    case midi_codec:decode_message(Status, Data) of
                        {ok, Message} ->
                            {ok, Message, next_running(Status, OldRunning), Rest};
                        {error, _} = Error ->
                            Error
                    end;
                _ ->
                    error({truncated_event, Status, DataAndRest})
            end;
        variable ->
            %% Unreachable: F0/F7 are framed before this is called.
            error({unexpected_variable_length, Status})
    end.

%% Running-status transition. Channel status (0x80-0xEF) becomes the new running
%% status; system real-time (0xF8-0xFF) is transparent to it; everything else
%% (system-common) clears it.
-spec next_running(0..255, 0..255 | undefined) -> 0..255 | undefined.
next_running(Status, _Old) when Status >= 16#80, Status =< 16#EF -> Status;
next_running(Status, Old)   when Status >= 16#F8                 -> Old;
next_running(_Status, _Old)                                      -> undefined.

%% Decode a VLQ-framed SysEx payload (shared by the F0 and F7 forms). The
%% trailing F7 terminator, when present, is stripped into #sysex.data.
-spec parse_sysex(binary()) ->
          {ok, #sysex{}, undefined, binary()} | {error, midierrs:reason()}.
parse_sysex(Bin) ->
    case read_vlq(Bin) of
        {ok, Len, Rest} ->
            case Rest of
                <<Payload:Len/binary, Rest2/binary>> ->
                    {ok, #sysex{data = strip_eox(Payload)}, undefined, Rest2};
                _ ->
                    error({truncated_sysex, Bin})
            end;
        {error, _} = Error ->
            Error
    end.

-spec strip_eox(binary()) -> binary().
strip_eox(<<>>) ->
    <<>>;
strip_eox(Payload) ->
    Size = byte_size(Payload),
    case binary:at(Payload, Size - 1) of
        ?STATUS_EOX -> binary:part(Payload, 0, Size - 1);
        _           -> Payload
    end.

%% Variable-length quantity reader: 1-4 bytes, 7 value bits each, high bit =
%% continuation. A 4-byte sequence whose 4th byte still sets the continuation
%% bit is malformed (S7) — {error, {bad_vlq, Bytes}}, never a silent value;
%% running out of bytes mid-quantity is the same error.
-spec read_vlq(binary()) -> {ok, non_neg_integer(), binary()} | {error, midierrs:reason()}.
read_vlq(Bin) ->
    read_vlq(Bin, 0, 0, Bin).

read_vlq(<<0:1, B:7, Rest/binary>>, Acc, _N, _Orig) ->
    {ok, (Acc bsl 7) bor B, Rest};
read_vlq(<<1:1, B:7, Rest/binary>>, Acc, N, Orig) when N < 3 ->
    read_vlq(Rest, (Acc bsl 7) bor B, N + 1, Orig);
read_vlq(<<1:1, _B:7, _Rest/binary>>, _Acc, 3, Orig) ->
    {error, {bad_vlq, vlq_prefix(Orig)}};
read_vlq(_Bin, _Acc, _N, Orig) ->
    {error, {bad_vlq, vlq_prefix(Orig)}}.

%% The (up to 4) leading bytes that form the offending VLQ, for the error term.
-spec vlq_prefix(binary()) -> binary().
vlq_prefix(Bin) ->
    binary:part(Bin, 0, min(4, byte_size(Bin))).

%%% ===========================================================================
%%% WRITE — slice B
%%% ===========================================================================
%% The writer is rebuilt in slice B against the canonical vocabulary (via
%% midi_codec:encode_message/1 + encode_meta/1). The 2010-vintage process-dict
%% writer is intentionally removed rather than carried as dead legacy code: it
%% would not compile against #seq{} and reintroduces the put/get state and the
%% legacy event tuples this arc deletes.
-spec write(#seq{}, file:name_all()) -> ok | {error, midierrs:reason()}.
write(#seq{}, _Path) ->
    {error, not_implemented}.
