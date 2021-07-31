-module(midilib).
-export([
    version/0,
    versions/0
]).

version() ->
    midilib_versions:lib().

versions() ->
    midilib_versions:all().
