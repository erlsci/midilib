-module(midibin).
-export([
    decode/1,
    encode/1
]).

-include_lib("midilib/include/errors.hrl").

-define(PITCH_BEND_MASK, 127).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Channel Voice Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(<<1:1, 0:3, Channel:4, 0:1, Pitch:7, 0:1, Velocity:7>>) ->
    {midi, {note_off, [{channel, Channel + 1},
                       {pitch, Pitch}, 
                       {velocity, Velocity}]}};

decode(<<1:1, 1:3, Channel:4, 0:1, Pitch:7, 0:1, Velocity:7>>) ->
    {midi, {note_on, [{channel, Channel + 1},
                      {pitch, Pitch},
                      {velocity, Velocity}]}};

decode(<<1:1, 2:3, Channel:4, 0:1, Controller:7, 0:1, Value:7>>) ->
    {midi, {poly_aftertouch, [{channel, Channel + 1},
                              {controller, Controller},
                              {value, Value}]}};

decode(<<1:1, 3:3, Channel:4, 0:1, Pitch:7, 0:1, Value:7>>) ->
    {midi, {cc, [{channel, Channel + 1},
                 {pitch, Pitch},
                 {value, Value}]}};

decode(<<1:1, 4:3, Channel:4, 0:1, Program:7>>) ->
    {midi, {program_change, [{channel, Channel + 1},
                             {program, Program}]}};

decode(<<1:1, 5:3, Channel:4, 0:1, Value:7>>) ->
    {midi, {aftertouch, [{channel, Channel + 1},
                         {pressure, Value}]}};

decode(<<1:1, 6:3, Channel:4, 0:1, Lsb:7, 0:1, Msb:7>>) ->
    {midi, {pitch_bend, [{channel, Channel + 1},
                         {value, (Msb bsl 7) + Lsb}]}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Channel Mode Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% All sound off

%% Reset all controllers

%% Local controll

%% All notes off

%% Omni mode off

%% Omni mode on

%% Poly mode off / Mono mode on

%% Poly mode on / Mono mode off

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(<<1:1, 7:3, 1:4, 0:1, MessageType:3, Value:4>>) ->
    {midi, {time_code_quarter_frame, MessageType, Value}};

decode(<<1:1, 7:3, 2:4, 0:1, Lsb:7, 0:1, Msb:7>>) ->
    {midi, {song_position_pointer, (Msb bsl 7) + Lsb}};

decode(<<1:1, 7:3, 3:4, 0:1, Value:7>>) ->
    {midi, {song_select, Value}};

decode(<<1:1, 7:3, 6:4>>) ->
    {midi, tune_request};

decode(<<1:1, 7:3, 6:4>>) ->
    {midi, end_of_sys_ex};

%decode(<<254:8>>) ->
%    ;

%decode(<<255:8>>) ->
%    reset;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Real-Time Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(<<248:8>>) ->
    {midi, {realtime, clock}};

decode(<<250:8>>) ->
    {midi, {realtime, start}};

decode(<<251:8>>) ->
    {midi, {realtime, continue}};

decode(<<252:8>>) ->
    {midi, {realtime, stop}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System Exclusive Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(<<1:1, 7:3, 0:4, _Rest/binary>>=Data) ->
    {midi, {sys_ex, Data}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Unexpected Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(Bin) ->
    {unknown, Bin}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Channel Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Aftertouch

encode({midi, {aftertouch, [{channel, Channel},
                            {pressure, Pressure}]}}) ->
    <<1:1, 5:3, (Channel - 1):4, 0:1, Pressure:7>>;
encode({midi, {aftertouch, Pressure}}) ->
    encode({midi, {aftertouch, [{channel, 1},
                                {pressure, Pressure}]}});

%% Note off

encode({midi, {note_off, [{channel, Channel},
                          {pitch, Pitch},
                          {velocity, Velocity}]}}) ->
    <<1:1, 0:3, (Channel - 1):4, 0:1, Pitch:7, 0:1, Velocity:7>>;
encode({midi, {note_off, [{pitch, Pitch},
                          {velocity, Velocity}]}}) ->
    encode({midi, {note_off, [{channel, 1},
                              {pitch, Pitch},
                              {velocity, Velocity}]}});
encode({midi, {note_off, Pitch}}) ->
    encode({midi, {note_off, [{pitch, Pitch},
                              {velocity, 127}]}});

%% Note on

encode({midi, {note_on, [{channel, Channel},
                         {pitch, Pitch},
                         {velocity, Velocity}]}}) ->
    ?ERR_NOT_IMPL;

%% Pitchbend

encode({midi, {pitchbend, Data}}) ->
    encode({midi, {pitch_bend, Data}});
encode({midi, {pitch_bend, [{channel, Channel},
                            {msb, Msb},
                            {lsb, Lsb}]}}) ->
    <<1:1, 6:3, (Channel - 1):4, 0:1, Lsb:7, 0:1, Msb:7>>;
encode({midi, {pitch_bend, [{channel, Channel},
                            {value, Value}]}}) ->
    Msb = (Value band (?PITCH_BEND_MASK bsl 7)) bsr 7,
    Lsb = Value band ?PITCH_BEND_MASK,
    encode({midi, {pitch_bend, [{channel, Channel},
                                {msb, Msb},
                                {lsb, Lsb}]}});
encode({midi, {pitch_bend, Value}}) ->
    encode({midi, {pitch_bend, [{channel, 1},
                                {value, Value}]}});

%% Poly-aftertouch

encode({midi, {poly_aftertouch, [{pitch, Pitch},
                                 {pressure, Pressure}]}}) ->
    ?ERR_NOT_IMPL;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Control Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode({midi, {bank_select_msb, Value}}) ->
    ?ERR_NOT_IMPL;
encode({midi, {bank_select_lsb, Value}}) ->
    ?ERR_NOT_IMPL;
encode({midi, {cc, [{controller, Controller},
                    {value, Value}]}}) ->
    ?ERR_NOT_IMPL;
encode({midi, {program_change, Program}}) ->
    ?ERR_NOT_IMPL;
encode({midi, {program_change, [{program, Program}]}}) ->
    ?ERR_NOT_IMPL;
encode({midi, {program_change, [{channel, Channel},
                                {program, Program}]}}) ->
    ?ERR_NOT_IMPL;
encode({midi, {reset, [{bank, Bank},
                       {program, Program}]}}) ->
    ?ERR_NOT_IMPL;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Real-Time Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode({midi, {realtime, clock}}) ->
    ?ERR_NOT_IMPL;
encode({midi, {realtime, continue}}) ->
    ?ERR_NOT_IMPL;
encode({midi, {realtime, reset}}) ->
    ?ERR_NOT_IMPL;
encode({midi, {realtime, start}}) ->
    ?ERR_NOT_IMPL;
encode({midi, {realtime, stop}}) ->
    ?ERR_NOT_IMPL;
encode({midi, {realtime, tick}}) ->
    ?ERR_NOT_IMPL;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System Exclusive Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode({midi, {sys_ex, Data}}) ->
    ?ERR_NOT_IMPL;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Unexpected Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode({midi, Data}) ->
    ?ERR_MIDI_UNSUP;
encode(Msg) ->
    ?ERR_NON_MIDI.
