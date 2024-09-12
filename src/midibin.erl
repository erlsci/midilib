-module(midibin).
-export([
    decode/1,
    encode/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Channel Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(<<1:1, 0:3, Chan:4, 0:1, ControllerNum:7, 0:1, Value:7>>) ->
    {note_off, Chan + 1, ControllerNum, Value};

decode(<<1:1, 1:3, Chan:4, 0:1, ControllerNum:7, 0:1, Value:7>>) ->
    {note_on, Chan + 1, ControllerNum, Value};

decode(<<1:1, 2:3, Chan:4, 0:1, ControllerNum:7, 0:1, Value:7>>) ->
    {aftertouch, Chan + 1, ControllerNum, Value};

decode(<<1:1, 6:3, Chan:4, 0:1, LSBs:7, 0:1, MSBs:7>>) ->
    {pitch_bend, Chan + 1, (MSBs bsl 7) + LSBs};




decode(<<1:1, 5:3, Chan:4, 0:1, Value:7>>) ->
    {channel_pressure, Chan + 1, Value};

decode(<<1:1, 7:3, 1:4, 0:1, MessageType:3, Value:4>>) ->
    {time_code_quarter_frame, MessageType, Value};

decode(<<1:1, 7:3, 2:4, 0:1, LSBs:7, 0:1, MSBs:7>>) ->
    {song_position_pointer, (MSBs bsl 7) + LSBs};

decode(<<1:1, 7:3, 3:4, 0:1, Value:7>>) ->
    {song_select, Value};

decode(<<1:1, 7:3, 6:4>>) ->
    tune_request;

decode(<<254:8>>) ->
    active_sensing;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Control Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(<<1:1, 3:3, Chan:4, 0:1, Controller:7, 0:1, Value:7>>) ->
    {midi, {cc, [{channel, Chan + 1},
                 {controller, Controller},
                 {value, Value}]}};

decode(<<1:1, 4:3, Chan:4, 0:1, Program:7>>) ->
    {midi, {program_change, [{channel, Chan + 1},
                             {program, Program}]}};

decode(<<255:8>>) ->
    reset;

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

decode(<<Bin/binary>>) ->
    {unknown, Bin}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Channel Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode({midi, {aftertouch, Pressure}}) ->
    'not-implemented';
encode({midi, {note_off, Pitch}}) ->
    'not-implemented';
encode({midi, {note_off, [{pitch, Pitch},
                          {velocity, Velocity}]}}) ->
    'not-implemented';
encode({midi, {note_on, [{pitch, Pitch},
                         {velocity, Velocity}]}}) ->
    'not-implemented';
encode({midi, {pitchbend, Value}}) ->
    'not-implemented';
encode({midi, {poly_aftertouch, [{pitch, Pitch},
                                 {pressure, Pressure}]}}) ->
    'not-implemented';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Control Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode({midi, {bank_select_msb, Value}}) ->
    'not-implemented';
encode({midi, {bank_select_lsb, Value}}) ->
    'not-implemented';
encode({midi, {cc, [{controller, Controller},
                    {value, Value}]}}) ->
    'not-implemented';
encode({midi, {program_change, Program}}) ->
    'not-implemented';
encode({midi, {program_change, [{program, Program}]}}) ->
    'not-implemented';
encode({midi, {program_change, [{channel, Channel},
                                {program, Program}]}}) ->
    'not-implemented';
encode({midi, {reset, [{bank, Bank},
                       {program, Program}]}}) ->
    'not-implemented';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Real-Time Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode({midi, {realtime, clock}}) ->
    'not-implemented';
encode({midi, {realtime, continue}}) ->
    'not-implemented';
encode({midi, {realtime, reset}}) ->
    'not-implemented';
encode({midi, {realtime, start}}) ->
    'not-implemented';
encode({midi, {realtime, stop}}) ->
    'not-implemented';
encode({midi, {realtime, tick}}) ->
    'not-implemented';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% System Exclusive Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode({midi, {sys_ex, Data}}) ->
    'not-implemented';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Unexpected Messages %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode({midi, Data}) ->
    'not-implemented';
encode(Msg) ->
    'not-implemented'.
