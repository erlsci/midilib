-module(midibin).
-export([
    decode/1,
    encode/1
]).

-include_lib("midilib/include/errors.hrl").

-define(MLSB_MASK, 127).

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

decode(<<1:1, 2:3, Channel:4, 0:1, Pitch:7, 0:1, Pressure:7>>) ->
    {midi, {poly_aftertouch, [{channel, Channel + 1},
                              {pitch, Pitch},
                              {pressure, Pressure}]}};

decode(<<1:1, 3:3, Channel:4, 0:1, Pitch:7, 0:1, Value:7>>) ->
    {midi, {cc, [{channel, Channel + 1},
                 {pitch, Pitch},
                 {value, Value}]}};

decode(<<1:1, 4:3, Channel:4, 0:1, Program:7>>) ->
    {midi, {program_change, [{channel, Channel + 1},
                             {program, Program}]}};

decode(<<1:1, 5:3, Channel:4, 0:1, Pressure:7>>) ->
    {midi, {aftertouch, [{channel, Channel + 1},
                         {pressure, Pressure}]}};

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System Common Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(<<1:1, 7:3, 1:4, 0:1, MessageType:3, Value:4>>) ->
    {midi, {time_code_quarter_frame, MessageType, Value}};

decode(<<1:1, 7:3, 2:4, 0:1, Lsb:7, 0:1, Msb:7>>) ->
    {midi, {song_position_pointer, (Msb bsl 7) + Lsb}};

decode(<<1:1, 7:3, 3:4, 0:1, Value:7>>) ->
    {midi, {song_select, Value}};

decode(<<1:1, 7:3, 6:4>>) ->
    {midi, tune_request};

decode(<<1:1, 7:3, 7:4>>) ->
    {midi, end_of_sys_ex};

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

decode(<<254:8>>) ->
    {midi, {realtime, active_sensing}};

decode(<<255:8>>) ->
    {midi, {realtime, reset}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System Exclusive Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(<<1:1, 7:3, 0:4, 0:1, Data:7>>) ->
    {midi, {sys_ex, Data}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Unexpected Messages & Errors %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode({error, _}=Error) ->
    Error;

decode(Bin) ->
    {unknown, Bin}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Channel Voice Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    <<1:1, 1:3, (Channel - 1):4, 0:1, Pitch:7, 0:1, Velocity:7>>;
encode({midi, {note_on, [{pitch, Pitch},
                         {velocity, Velocity}]}}) ->
    encode({midi, {note_on, [{channel, 1},
                             {pitch, Pitch},
                             {velocity, Velocity}]}});

%% Pitchbend

encode({midi, {pitchbend, Data}}) ->
    encode({midi, {pitch_bend, Data}});
encode({midi, {pitch_bend, [{channel, Channel},
                            {msb, Msb},
                            {lsb, Lsb}]}}) ->
    <<1:1, 6:3, (Channel - 1):4, 0:1, Lsb:7, 0:1, Msb:7>>;
encode({midi, {pitch_bend, [{channel, Channel},
                            {value, Value}]}}) ->
    Msb = (Value band (?MLSB_MASK bsl 7)) bsr 7,
    Lsb = Value band ?MLSB_MASK,
    encode({midi, {pitch_bend, [{channel, Channel},
                                {msb, Msb},
                                {lsb, Lsb}]}});
encode({midi, {pitch_bend, Value}}) ->
    encode({midi, {pitch_bend, [{channel, 1},
                                {value, Value}]}});

%% Poly-aftertouch

encode({midi, {poly_aftertouch, [{channel, Channel},
                                 {pitch, Pitch},
                                 {pressure, Pressure}]}}) ->
    <<1:1, 2:3, (Channel - 1):4, 0:1, Pitch:7, 0:1, Pressure:7>>;
encode({midi, {poly_aftertouch, [{pitch, Pitch},
                                 {pressure, Pressure}]}}) ->
    encode({midi, {poly_aftertouch, [{channel, 1},
                                     {pitch, Pitch},
                                     {pressure, Pressure}]}});

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System Common Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% time_code_quarter_frame

encode({midi, {time_code_quarter_frame, MessageType, Value}}) ->
    <<1:1, 7:3, 1:4, 0:1, MessageType:3, Value:4>>;

%% song_position_pointer

encode({midi, {song_position_pointer, [{msb, Msb},
                                       {lsb, Lsb}]}}) ->
    <<1:1, 7:3, 2:4, 0:1, Lsb:7, 0:1, Msb:7>>;
encode({midi, {song_position_pointer, Value}}) ->
    Msb = (Value band (?MLSB_MASK bsl 7)) bsr 7,
    Lsb = Value band ?MLSB_MASK,
    encode({midi, {song_position_pointer, [{msb, Msb}, {lsb, Lsb}]}});

%% song_select

encode({midi, {song_select, Value}}) ->
    <<1:1, 7:3, 3:4, 0:1, Value:7>>;

%% tune_request

encode({midi, tune_request}) ->
    <<1:1, 7:3, 6:4>>;

%% end_of_sys_ex

encode({midi, end_of_sys_ex}) ->
    <<1:1, 7:3, 7:4>>;

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

encode({midi, {realtime, active_sensing}}) ->
    <<254:8>>;
encode({midi, {realtime, clock}}) ->
    <<248:8>>;
encode({midi, {realtime, continue}}) ->
    <<251:8>>;
encode({midi, {realtime, reset}}) ->
    <<255:8>>;
encode({midi, {realtime, start}}) ->
    <<250:8>>;
encode({midi, {realtime, stop}}) ->
    <<252:8>>;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System Exclusive Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode({midi, {sys_ex, Data}}) ->
    <<1:1, 7:3, 0:4, 0:1, Data:7>>;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Unexpected Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode({midi, Data}) ->
    ?ERR_MIDI_UNSUP;
encode(Msg) ->
    ?ERR_NON_MIDI.
