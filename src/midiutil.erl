-module(midiutil).
-export([
    bpm_to_mpq/1,
    event_to_text/1, event_to_text/2,
    mpq_to_bpm/1,
    note_length/1,
    note_names/0,
    note_to_string/1,
    quantize/2,
    seq_to_text/1, seq_to_text/2,
    track_to_text/1, track_to_text/2]).
-author("Jim Menard, jim@jimmenard.com").

note_names() ->
    ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"].

note_length(whole) -> 4.0;
note_length(half) -> 2.0;
note_length(quarter) -> 1.0;
note_length(eighth) -> 0.5;
note_length('8th') -> 0.5;
note_length(sixteenth) -> 0.25;
note_length('16th') -> 0.25;
note_length(thirtysecond) -> 0.125;
note_length('thirty second') -> 0.125;
note_length('32nd') -> 0.125;
note_length(sixtyfourth) -> 0.0625;
note_length('sixty fourth') -> 0.0625;
note_length('64th') -> 0.0625.

-define(MICROSECS_PER_MINUTE, 1000000 * 60).

%% Translates beats per minute to microseconds per quarter note (beat).
bpm_to_mpq(Bpm) ->
    ?MICROSECS_PER_MINUTE / Bpm.

%% Translates microseconds per quarter note (beat) to beats per minute.
mpq_to_bpm(Mpq) ->
    ?MICROSECS_PER_MINUTE / Mpq.

%% Quantize a lists's event's delta times by returning a new list of events
%% where the delta time of each is moved to the nearest multiple of Boundary.
quantize({track, ListOfEvents}, Boundary) ->
    quantize(ListOfEvents, Boundary);
quantize([], _Boundary) ->
    [];
quantize(ListOfEvents, Boundary) ->
    {NewListOfEvents, _} =
	lists:mapfoldl(fun(E, BeatsFromStart) ->
			       quantized_event(E, BeatsFromStart, Boundary)
		       end,
		       0, ListOfEvents),
    NewListOfEvents.

%% Return a tuple containing a quantized copy of Event and the beats from
%% the start of this event before it was quantized.
quantized_event(Event, BeatsFromStart, Boundary) ->
    io:format("qe ~p, ~p, ~p~n", [Event, BeatsFromStart, Boundary]),
    {Name, DeltaTime, Values} = Event,
    NewDeltaTime = quantized_delta_time(BeatsFromStart, DeltaTime, Boundary),
    {{Name, NewDeltaTime, Values}, BeatsFromStart + DeltaTime}.

quantized_delta_time(BeatsFromStart, DeltaTime, Boundary) ->
    Diff = (BeatsFromStart + DeltaTime) div Boundary,
    if
        Diff >= Boundary / 2 ->
            DeltaTime - Diff;
        true ->
            DeltaTime - Diff + Boundary
    end.

%% Given a MIDI note number, return the name and octave as a string.
note_to_string(Num) ->
    Note = Num rem 12,
    Octave = Num div 12,
    lists:concat([lists:nth(Note + 1, note_names()), Octave - 1]).

seq_to_text(Seq) ->
    seq_to_text(Seq, false).
seq_to_text({seq, _, Tracks}, ShowChanEvents) ->
    seq_to_text({seq, Tracks}, ShowChanEvents);
seq_to_text({seq, _Header, _MetaTrack, Tracks}, ShowChanEvents) ->
    seq_to_text({seq, Tracks}, ShowChanEvents);
seq_to_text({seq, Tracks}, ShowChanEvents) ->
    lists:map(fun(T) -> track_to_text(T, ShowChanEvents) end, Tracks),
    ok.

track_to_text(Track) ->
    track_to_text(Track, false).
track_to_text(Track, ShowChanEvents) ->
    io:format("~n*** Track start ***~n~n"),
    {track, Events} = Track,
    lists:map(fun(E) -> event_to_text(E, ShowChanEvents) end, Events),
    ok.

event_to_text(Event) ->
    event_to_text(Event, false).
event_to_text({Name, _} = Event, ShowChanEvents) ->
    event_to_text(Event, Name, ShowChanEvents);
event_to_text({Name, _, _} = Event, ShowChanEvents) ->
    event_to_text(Event, Name, ShowChanEvents).
event_to_text(Event, Name, ShowChanEvents) ->
    IsChanEvent = lists:member(Name, [off, on, poly_press, controller, program,
                                      chan_press, pitch_bend]),
    if
	Name == track_end ->
            ok;
        ShowChanEvents; not IsChanEvent ->
	    io:format("~p~n", [Event]);
        true ->
            io:format("~p~n", [Name])
    end.
