-module(midimsg).
-export([
    batch/1
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
    aftertouch/1,
    note_off/1,
    note_off_velocity/2,
    note_on/2,
    pitchbend/1,
    poly_aftertouch/2,
    program_change/1
]).
-export([
    cc/2,
    reset/2
]).

batch(Msgs) ->
    {midi, {batch, Msgs}}.

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
keysig(Key, IsMajor, Num, IsFlat) ->
    {midi, {keysig, [{key, Key},
                     {is_major, IsMajor},
                     {num, Num},
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
%%%%% Regular Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec aftertouch (integer()) -> tuple().
aftertouch(Pressure) ->
    {midi, {aftertouch, Pressure}}.

-spec note_off (integer()) -> tuple().
note_off(Pitch) ->
    {midi, {note_off, Pitch}}.

-spec note_off_velocity (integer(), integer()) -> tuple().
note_off_velocity(Pitch, Velocity) ->
    {midi, {note_off, [{pitch, Pitch},
                       {velocity, Velocity}]}}.

-spec note_on (integer(), integer()) -> tuple().
note_on(Pitch, Velocity) ->
    {midi, {note_on, [{pitch, Pitch},
                      {velocity, Velocity}]}}.

-spec pitchbend (integer()) -> tuple().
pitchbend(Value) ->
    {midi, {pitchbend, Value}}.

-spec program_change (integer()) -> tuple().
program_change(Program) ->
    {midi, {program_change, Program}}.

-spec poly_aftertouch (integer(), integer()) -> tuple().
poly_aftertouch(Pitch, Pressure) ->
    {midi, {poly_aftertouch, [{pitch, Pitch},
                              {pressure, Pressure}]}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Control Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Resets server to some established defaults using the giving bank and program
-spec reset (integer(), integer()) -> tuple().
reset(Bank, Program) ->
    {midi, {reset, [{bank, Bank},
                    {program, Program}]}}.

-spec cc (integer(), integer()) -> tuple().
cc(Controller, Value) ->
    {midi, {cc, [{controller, Controller},
                 {value, Value}]}}.
