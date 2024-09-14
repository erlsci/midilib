%%%% The messages created by the functions in this module were originally
%%%% designed to interoperate with a Golang MIDI server (see 
%%%% https://github.com/geomyidia/midiserver/) via Erlang Ports, then later
%%%% the Erlang exec project. The underlying MIDI library has messages defined
%%%% here:
%%%%   * https://gitlab.com/gomidi/midi/-/blob/master/writer/messages.go
%%%% and it was the function arguments defined there that shaped the messages
%%%% in this module, with some corrections on terminology (it's "pitch" not
%%%% "key").
%%%%
%%%% Since then, the scope has increased and this module's messages have become
%%%% the lingua franca for a collection of music projects (see
%%%% https://github.com/ut-proj). Furthermore, this module has served as the
%%%% source and destination for the binary encode and decode functions,
%%%% respectively, from the new 'midibin' module, supporting raw binary MIDI
%%%% messages.
-module(midimsg).
-export([
    batch/1,
    batch/2
]).
-export([
    copyright/1,
    cuepoint/1,
    device/1,
    instrument/1,
    keysig/4,
    lyric/1,
    marker/1,
    meter/2,
    program/1,
    sequence_number/1,
    sequencer_data/1,
    smpte/5,
    tempo_bpm/1,
    text/1,
    time_sig/4,
    track_sequence_name/1,
    undefined/1
]).
-export([
    aftertouch/1, aftertouch/2,
    note_off/1, note_off/2, note_off/3,
    note_on/2, note_on/3,
    pitch_bend/1, pitch_bend/2, pitch_bend/3,
    poly_aftertouch/2, poly_aftertouch/3
]).
-export([
    rt_clock/0,
    rt_continue/0,
    rt_reset/0,
    rt_start/0,
    rt_stop/0,
    rt_tick/0
]).
-export([
    cc/2,
    program_change/1, program_change/2
]).
-export([
    sys_ex/1
]).

%% DEPRECATED
-export([
    bank_select/3,
    bank_select/4,
    bank_select_msb/1,
    bank_select_lsb/1,
    channel/1,
    reset/2
]).

%% Aliases for LFE developers
-export([
    'sequence-number'/1,
    'sequencer-data'/1,
    'tempo-bpm'/1,
    'tempo'/1,    
    'time-sig'/4,
    'track-sequence-name'/1,
    'note-off'/1, 'note-off'/2, 'note-off'/3,
    'note-on'/2, 'note-on'/3,
    'poly-aftertouch'/2, 'poly-aftertouch'/3,
    'program-change'/1, 'program-change'/2,
    'bank-select'/3,
    'bank-select'/4,
    'bank-select-msb'/1,
    'bank-select-lsb'/1,
    'rt-clock'/0,
    'rt-continue'/0,
    'rt-reset'/0,
    'rt-start'/0,
    'rt-stop'/0,
    'rt-tick'/0
]).

batch(Msgs) ->
    batch(Msgs, [{id, uuid:get_v4_urandom()}]).

%% Iterate through all the Msg (tuples with 'midi' as the first element),
%% drop the first element, add the second to a list, and put that list into
%% the payload.
batch(Msgs, Metadata) ->
    Batch = [Payload || {midi, Payload} <- Msgs],
    {midi, {batch, lists:append(Metadata, [{messages, Batch}])}}.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Meta Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% Creates the copyright meta message
copyright(Text) ->
    {midi, {copyright, Text}}.

%% Creates the cuepoint meta message
cuepoint(Text) ->
    {midi, {cuepoint, Text}}.

%% Creates the device port meta message
-spec device (list()) -> tuple().
device(Port) ->
    {midi, {device, Port}}.

%% Creates the instrument name meta message
instrument(Name) ->
    {midi, {instrument, Name}}.

%% Creates the key signature meta message
-spec keysig (integer(), boolean(), integer(), boolean()) -> tuple().
keysig(Key, IsMajor, AccidentalCount, IsFlat) ->
    {midi, {keysig, [{key, Key},
                     {is_major, IsMajor},
                     {num, AccidentalCount},
                     {is_flat, IsFlat}]}}.

%% Creates the lyric meta message
lyric(Text) ->
    {midi, {lyric, Text}}.

%% Creates the marker meta message
marker(Text) ->
    {midi, {marker, Text}}.

%% Creates the time signature meta message (more easily than time_sig)
-spec meter (integer(), integer()) -> tuple().
meter(Numerator, Denominator) ->
    {midi, {meter, [{numerator, Numerator},
                    {denominator, Denominator}]}}.

%% Creates the program meta message
program(Text) ->
    {midi, {program, Text}}.

%% Creates the sequence_number meta message
sequence_number(Text) ->
    {midi, {sequence_number, Text}}.

%% Creates the sequencer_data meta message
-spec sequencer_data (binary()) -> tuple().
sequencer_data(Bytes) ->
    {midi, {sequencer_data, Bytes}}.

%% Creates the SMPTE meta message
-spec smpte (binary(), binary(), binary(), binary(), binary()) -> tuple().
smpte(Hour, Minute, Second, Frame, FractionalFrame) ->
    {midi, {smpte, [{hour, Hour},
                    {minute, Minute},
                    {second, Second},
                    {frame, Frame},
                    {fractional_frame, FractionalFrame}]}}.

%% Creates the tempo_bpm meta message
-spec tempo_bpm (float()) -> tuple().
tempo_bpm(Bpm) ->
    {midi, {tempo_bpm, Bpm}}.

%% Creates the text meta message
text(Text) ->
    {midi, {text, Text}}.

%% Creates the time signature meta message
-spec time_sig (integer(), integer(), integer(), integer()) -> tuple().
time_sig(Numerator, Denominator, ClocksPerClick, DemiSemiQuaverPerQuarter) ->
    {midi, {time_sig, [{numerator, Numerator},
                       {denominator, Denominator},
                       {cpc, ClocksPerClick},
                       {dsqpq, DemiSemiQuaverPerQuarter}]}}.

%% Creates the track_sequence_name meta message
track_sequence_name(Text) ->
    {midi, {track_sequence_name, Text}}.

%% Creates the undefined meta message
undefined(Bytes) ->
    {midi, {undefined, Bytes}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Channel Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec aftertouch (integer()) -> tuple().
aftertouch(Pressure) ->
    {midi, {aftertouch, Pressure}}.
aftertouch(Channel, Pressure) ->
    {midi, {aftertouch, [{channel, Channel},
                         {pressure, Pressure}]}}.

-spec note_off (integer()) -> tuple().
note_off(Pitch) ->
    {midi, {note_off, Pitch}}.
-spec note_off (integer(), integer()) -> tuple().
note_off(Pitch, Velocity) ->
    {midi, {node_off, [{pitch, Pitch},
                       {velocity, Velocity}]}}.
note_off(Channel, Pitch, Velocity) -> 
    {midi, {node_off, [{channel, Channel},
                       {pitch, Pitch},
                       {velocity, Velocity}]}}.

-spec note_on (integer(), integer()) -> tuple().
note_on(Pitch, Velocity) ->
    {midi, {note_on, [{pitch, Pitch},
                      {velocity, Velocity}]}}.
note_on(Channel, Pitch, Velocity) ->
    {midi, {note_on, [{channel, Channel},
                      {pitch, Pitch},
                      {velocity, Velocity}]}}.

-spec pitch_bend (integer()) -> tuple().
pitch_bend(Value) ->
    {midi, {pitch_bend, Value}}.
pitch_bend(Channel, Value) ->
    {midi, {pitch_bend, [{channel, Channel},
                         {value, Value}]}}.
pitch_bend(Channel, Msb, Lsb) ->
    {midi, {pitch_bend, [{channel, Channel},
                         {msb, Msb},
                         {lsb, Lsb}]}}.

-spec poly_aftertouch (integer(), integer()) -> tuple().
poly_aftertouch(Pitch, Pressure) ->
    {midi, {poly_aftertouch, [{pitch, Pitch},
                              {pressure, Pressure}]}}.
poly_aftertouch(Channel, Pitch, Pressure) ->
    {midi, {poly_aftertouch, [{channel, Channel},
                              {pitch, Pitch},
                              {pressure, Pressure}]}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Control Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec cc (integer(), integer()) -> tuple().
cc(Controller, Value) ->
    {midi, {cc, [{controller, Controller},
                 {value, Value}]}}.

-spec program_change (integer()) -> tuple().
program_change(Program) ->
    {midi, {program_change, Program}}.

program_change(Channel, Program) ->
    {midi, {program_change, [{channel, Channel},
                             {program, Program}]}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Real-Time Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec rt_clock () -> tuple().
rt_clock() ->
    {midi, {realtime, clock}}.

-spec rt_continue () -> tuple().
rt_continue() ->
    {midi, {realtime, continue}}.

-spec rt_reset () -> tuple().
rt_reset() ->
    {midi, {realtime, reset}}.

-spec rt_start () -> tuple().
rt_start() ->
    {midi, {realtime, start}}.

-spec rt_stop () -> tuple().
rt_stop() ->
    {midi, {realtime, stop}}.

-spec rt_tick () -> tuple().
rt_tick() ->
    {midi, {realtime, tick}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System Exclusive Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sys_ex (binary()) -> tuple().
sys_ex(Data) ->
    {midi, {sys_ex, Data}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Aliases for LFE Users %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'sequence-number'(X) ->
    sequence_number(X).
'sequencer-data'(X) ->
    sequencer_data(X).
'tempo-bpm'(X) ->
    tempo_bpm(X).
tempo(X) ->
    tempo_bpm(X).
'time-sig'(W, X, Y, Z) ->
    time_sig(W, X, Y, Z).
'track-sequence-name'(X) ->
    track_sequence_name(X).
'note-off'(X) ->
    note_off(X).
'note-off'(X, Y) ->
    note_off(X, Y).
'note-off'(X, Y, Z) ->
    note_off(X, Y, Z).
'note-on'(X, Y) ->
    note_on(X, Y).
'note-on'(X, Y, Z) ->
    note_on(X, Y, Z).
'poly-aftertouch'(X, Y) ->
    poly_aftertouch(X, Y).
'poly-aftertouch'(X, Y, Z) ->
    poly_aftertouch(X, Y, Z).
'program-change'(X) ->
    program_change(X).
'program-change'(X, Y) ->
    program_change(X, Y).
'rt-clock'() ->
    rt_clock().
'rt-continue'() ->
    rt_continue().
'rt-reset'() ->
    rt_reset().
'rt-start'() ->
    rt_start().
'rt-stop'() ->
    rt_stop().
'rt-tick'() ->
    rt_tick().

%%%%%%%%%%%%%%%%%%%%%%
%%%%% Deprecated %%%%%
%%%%%%%%%%%%%%%%%%%%%%

-spec bank_select (integer(), integer(), integer()) -> list().
bank_select(MsbValue, LsbValue, ProgChValue) ->
    batch([bank_select_msb(MsbValue),
           bank_select_lsb(LsbValue),
           program_change(ProgChValue)]).

bank_select(MsbValue, LsbValue, ProgChValue, Metadata) ->
    batch([bank_select_msb(MsbValue),
           bank_select_lsb(LsbValue),
           program_change(ProgChValue)],
         Metadata).

-spec bank_select_msb (integer()) -> tuple().
bank_select_msb(Value) ->
    {midi, {bank_select_msb, Value}}.

-spec bank_select_lsb (integer()) -> tuple().
bank_select_lsb(Value) ->
    {midi, {bank_select_lsb, Value}}.

channel(ChannelId) ->
    {midi, {channel, ChannelId}}.

%% Resets server to some established defaults using the given bank and program
-spec reset (integer(), integer()) -> tuple().
reset(Bank, Program) ->
    {midi, {reset, [{bank, Bank},
                    {program, Program}]}}.

'bank-select'(X, Y, Z) ->
    bank_select(X, Y, Z).
'bank-select'(W, X, Y, Z) ->
    bank_select(W, X, Y, Z).
'bank-select-msb'(X) ->
    bank_select_msb(X).
'bank-select-lsb'(X) ->
    bank_select_lsb(X).
