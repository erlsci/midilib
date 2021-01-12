-module(midiutil).
-export([seq_to_text/1, seq_to_text/2, track_to_text/1, track_to_text/2, event_to_text/1, event_to_text/2]).
-author("Jim Menard, jim@jimmenard.com").

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
