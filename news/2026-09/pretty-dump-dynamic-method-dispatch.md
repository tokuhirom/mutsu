# PrettyDump reaches parity through dispatch and container fixes

`PrettyDump` 1.2.3 exposed a method-dispatch gap: when a quoted dynamic method
call such as `self."$name"(...)` used a name also present in mutsu's native
method table, mutsu selected the native method before the user-defined method.
Static and variable-receiver calls already preserved the user override, but the
non-mutating dynamic receiver path did not. The distribution also exposed two
container boundaries: positional construction of an `is Hash` subclass lost its
key/value pairs, and `Match.hash` returned mutsu's internal capture `Hash`
instead of the public immutable `Map`.

The VM now applies the same user-method precedence to quoted dynamic calls,
preserves positional entries in Hash-subclass backing storage, and tags the
public Match capture view as a Map. The distribution's collection, pair, match,
subclass, and string rendering tests therefore use their `PrettyDump` overrides
and reach parity.
