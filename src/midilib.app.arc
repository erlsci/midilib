{application, midilib, [
    {description, "Erlang MIDI Library"},
    {vsn, "0.2.0"},
    {registered, []},
    {applications, [
        kernel,
        stdlib
    ]},
    {modules, [
        midifile,
        midilib,
        midiutil
    ]},
    {licenses, ["MIT"]},
    {maintainers, ["Duncan McGreggor"]},
    {links, [
        {"Github", "https://github.com/erlsci/midilib"}
    ]}
]}.
