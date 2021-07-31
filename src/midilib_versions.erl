-module(midilib_versions).
-export([
     all/0,
     arch/0,
     langs/0,
     lib/0, lib/1,
     name/1,
     rebar/0
]).

lib() -> lib(midilib).

lib(AppName) ->
    application:load(AppName),
    case application:get_key(AppName, vsn) of
        {ok, Vsn} -> Vsn;
        Default -> Default
    end.

arch() ->
    {architecture, erlang:system_info(system_architecture)}.

name(AppName) ->
    {AppName, lib(AppName)}.

rebar() ->
    [name(rebar)].

langs() ->
    [{erlang, erlang:system_info(otp_release)},
     {emulator, erlang:system_info(version)},
     {driver, erlang:system_info(driver_version)}].

all() ->
    lists:append([[name(midilib)],
                  langs(),
                  rebar(),
                  [arch()]]).

