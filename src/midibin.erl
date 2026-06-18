%%% ===========================================================================
%%% midibin — binary <-> canonical-message codec for live (wire) MIDI.
%%%
%%% Works in the canonical vocabulary (include/midi_msg.hrl): bare records, no
%%% `{midi, _}` envelope, 1-based channels at the API boundary (the 0..15 wire
%%% nibble is internal). One complete message per decode/encode call — no
%%% running-status expansion, no stream reassembly (the transport contract the
%%% family relies on, NEEDS R1).
%%%
%%% midibin is now a thin WIRE FRAMER over midi_codec (the shared, framing-free
%%% message<->bytes core). It owns only what is wire-specific: F0 ... F7 SysEx
%%% framing and the one-complete-message-per-call split. The message body
%%% mapping (channel/system byte layout, ±1 channels, the C5 channel-mode
%%% policy, no-vel-0-fold) lives in midi_codec, so the wire and file codecs can
%%% never disagree on how a message is laid out.
%%% ===========================================================================
-module(midibin).
%% Binary <-> canonical-message MIDI codec. decode/1 turns one complete wire
%% message into a canonical record; encode/1 does the reverse. encode_batch/1 and
%% decode_batch/1 operate on lists. Failures are {error, midierrs:reason()};
%% render with midierrs:format_error/1.
%% Kept portable: the v0.6.0 support floor is OTP 22-29 (row 12), so the
%% OTP-27+ -moduledoc/-doc attributes are intentionally not used; module/function
%% docs stay in %%-comments and -spec.

-export([decode/1, encode/1, encode_batch/1, decode_batch/1]).

-include("include/midi_msg.hrl").

%%% ===========================================================================
%%% DECODE
%%% ===========================================================================
%% Decode one complete wire message into a canonical record. Input is always
%% wire bytes; unrecognised bytes are {error, {unknown, Bin}} (DESIGN §6). A
%% non-binary argument is a caller bug and crashes (EH-05) — `non_midi` is an
%% encode-only reason, not a decode outcome.
%%
%% SysEx is the one wire-framed message: F0 ... F7, arbitrary length. Everything
%% else is `<<Status, Data/binary>>` for the framing-free core; an unrecognised
%% status (or wrong data length) comes back as {unknown, <<Status, Data>>},
%% whose bytes are exactly the input — so the contract is preserved.
-spec decode(binary()) -> {ok, message()} | {error, {unknown, binary()}}.
decode(<<16#F0, Rest/binary>>) ->
    Size = byte_size(Rest),
    case Size > 0 andalso binary:at(Rest, Size - 1) =:= 16#F7 of
        true  -> {ok, #sysex{data = binary:part(Rest, 0, Size - 1)}};
        false -> {error, {unknown, <<16#F0, Rest/binary>>}}
    end;
decode(<<Status, Data/binary>>) ->
    midi_codec:decode_message(Status, Data);
decode(Bin) when is_binary(Bin) ->
    %% Only the empty binary reaches here (no status byte at all).
    {error, {unknown, Bin}}.

%%% ===========================================================================
%%% ENCODE
%%% ===========================================================================
%% Encode one canonical message into its wire bytes. SysEx gets its F0 ... F7
%% wire framing here; every other message is the core's {Status, Data} laid out
%% as a flat binary. Meta records, out-of-range fields, and non-messages fall
%% through midi_codec:encode_message/1 to {unsupported, _} / non_midi.
-spec encode(message() | term()) -> {ok, binary()} | {error, midierrs:reason()}.
encode(#sysex{data = D}) when is_binary(D) ->
    {ok, <<16#F0, D/binary, 16#F7>>};
encode(Msg) ->
    case midi_codec:encode_message(Msg) of
        {ok, {Status, Data}} -> {ok, <<Status, Data/binary>>};
        {error, _} = Error   -> Error
    end.

%%% ===========================================================================
%%% BATCH (list in, list out — no {midi,{batch,_}} envelope; O(n))
%%% ===========================================================================
%% Encode a list of messages to a list of binaries. Short-circuits on the first
%% un-encodable message, appending a single trailing {error, _} (the behaviour
%% `midi` relies on, NEEDS R9).
-spec encode_batch([message()]) -> [binary() | {error, midierrs:reason()}].
encode_batch(Msgs) -> encode_batch(Msgs, []).

encode_batch([], Acc) ->
    lists:reverse(Acc);
encode_batch([Msg | Rest], Acc) ->
    case encode(Msg) of
        {ok, Bin}      -> encode_batch(Rest, [Bin | Acc]);
        {error, _} = E -> lists:reverse([E | Acc])
    end.

%% Decode a list of complete-message binaries; one result per element.
-spec decode_batch([binary()]) -> [{ok, message()} | {error, term()}].
decode_batch(Bins) ->
    [decode(B) || B <- Bins].
