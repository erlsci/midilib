# midilib

*Erlang MIDI Library*

[![Build Status][gh-actions-badge]][gh-actions]
[![Erlang Versions][erlang-badge]][versions]
[![Tag][github-tag-badge]][github-tag]

[![Project Logo][logo]][logo-large]

## Setup

Include one of the following in your project's `rebar.config`: 

``` erlang
%% Latest Release
{deps, [
  {midilib, "0.4.1"}
]}.
```
or
``` erlang
%% Development
{deps, [
  {midilib, {git, "https://github.com/erlsci/midilib", {branch, "release/0.4.x"}}}
]}.
```

## `midimsg`

Functions for creating MIDI messages, designed to be used with `term_to_binary/1`
and sent to Ports-capabale servers.

## `midifile`

Reads and writes type 1 MIDI files (note that type 1 files may contain any number
of tracks that would be performed synchronously.

## `midilib_util`

Utility functions for handling note lengths, beats, quantization, and note
names, and more.

## Testing

Perform any checks / tests:

``` shell
$ rebar3 as test check
```

## Resources

* [MIDI](https://en.wikipedia.org/wiki/MIDI) on WikiPedia
* [MIDI 1.0 Specification](https://www.midi.org/specifications/item/the-midi-1-0-specification)
* [MIDI File Format Specification](https://www.midi.org/specifications/file-format-specifications/standard-midi-files)

## License

MIT License


[//]: ---Named-Links---

[logo]: priv/images/logo-v1-x250.png
[logo-large]: priv/images/logo-v1-x1000.png
[github]: https://github.com/erlsci/midilib
[gh-actions-badge]: https://github.com/erlsci/midilib/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/erlsci/midilib/actions
[erlang-badge]: https://img.shields.io/badge/erlang-19%20to%2023-blue.svg
[versions]: https://github.com/erlsci/osc/blob/master/.github/workflows/cicd.yml
[github-tag]: https://github.com/erlsci/midilib/tags
[github-tag-badge]: https://img.shields.io/github/tag/erlsci/midilib.svg
[github-downloads]: https://img.shields.io/github/downloads/erlsci/midilib/total.svg
