# Startup no longer declares every exception class's attributes

Since #18758 the built-in exception classes declare the attributes rakudo's
do (`X::NYI`'s `$.feature`, `X::OutOfRange`'s `$.range`, ...). Every
`mutsu` process declared all ~2,100 of them, for 337 classes, while building
its class table: it parsed the attribute table and created each declaration
and accessor row. That doubled the startup of a trivial script, and every
program paid it, whether or not it touched an exception.

A built-in exception class now declares its attributes the first time
anything reads or changes its attribute list: an accessor call, construction,
`.^attributes`, `.can`, or a subclass composing it. Until then the method
table answers that class's accessor lookups from the class itself, so
dispatch sees the same declarations as before. Startup also lost a few
hundred scratch allocations from registering the `X::` classes.

`say "hello"` (`benchmarks/bench-startup.raku`) went from 15.72M to about
8.0M instructions and from 19.5k to 10.6k allocations (callgrind/DHAT,
profiling build).
