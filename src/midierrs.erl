%%% ===========================================================================
%%% midierrs — midilib error vocabulary.
%%%
%%% Structured, matchable error reasons (atoms / tagged tuples) plus a single
%%% `format_error/1` that renders them as human text. Keeps the human strings
%%% OUT of the matchable channel (audit #16), and gives the formerly-empty
%%% module a real job (audit #24).
%%%
%%% Adopted by the codecs in arcs 3-5; the canonical return conventions are:
%%%   midibin:decode/1 -> {ok, message()} | {error, {unknown, binary()}}
%%%   midibin:encode/1 -> {ok, binary()}  | {error, reason()}
%%%   midifile:read/1  -> {ok, #seq{}}    | {error, reason()}
%%%   midifile:write/2 -> ok              | {error, reason()}
%%% ===========================================================================
-module(midierrs).
-moduledoc "midilib error vocabulary: structured reasons and `format_error/1`.".

-export([format_error/1]).

-export_type([reason/0]).

-type reason() ::
        non_midi                               %% term is not a MIDI message
      | not_implemented                        %% recognised but unsupported feature
      | {unsupported, term()}                  %% a known message we cannot encode
      | {unknown, binary()}                    %% decode: well-formed but unhandled bytes
      | {bad_vlq, binary()}                    %% read: malformed variable-length quantity
      | {bad_value, atom(), term()}            %% encode: a field is out of range
      | {open, file:name_all(), term()}        %% file:open/2 failed
      | {not_midi_file, file:name_all()}.      %% no MThd chunk found

%%% --------------------------------------------------------------------------
%%% API
%%% --------------------------------------------------------------------------

-doc "Render an error `reason()` as a human-readable string.".
-spec format_error(reason()) -> string().
format_error(non_midi) ->
    "not a MIDI message";
format_error(not_implemented) ->
    "not implemented";
format_error({unsupported, Term}) ->
    lists:flatten(io_lib:format("unsupported MIDI message: ~tp", [Term]));
format_error({unknown, Bin}) ->
    lists:flatten(io_lib:format("unrecognised MIDI bytes: ~tp", [Bin]));
format_error({bad_vlq, Bin}) ->
    lists:flatten(io_lib:format("malformed variable-length quantity: ~tp", [Bin]));
format_error({bad_value, Field, Value}) ->
    lists:flatten(io_lib:format("value out of range for ~ts: ~tp", [Field, Value]));
format_error({open, Path, PosixReason}) ->
    lists:flatten(io_lib:format("cannot open ~ts: ~ts",
                                [Path, file_reason(PosixReason)]));
format_error({not_midi_file, Path}) ->
    lists:flatten(io_lib:format("not a MIDI file (no MThd chunk): ~ts", [Path])).

%%% --------------------------------------------------------------------------
%%% Internal
%%% --------------------------------------------------------------------------

%% Render a file:open/2 posix-style reason, falling back for nested {error, _}.
-spec file_reason(term()) -> string().
file_reason({error, Posix}) ->
    file_reason(Posix);
file_reason(Posix) when is_atom(Posix) ->
    case file:format_error(Posix) of
        "unknown POSIX error" ++ _ -> atom_to_list(Posix);
        Str                        -> Str
    end;
file_reason(Other) ->
    lists:flatten(io_lib:format("~tp", [Other])).
