# `Str.naive-word-wrapper`

Rakudo's core `Str` carries an implementation-detail word wrapper, `.naive-word-wrapper(:max, :indent)`.
Vendored core modules call it; upstream `NativeCall.rakumod` and `NativeCall/Types.rakumod` use it to
format their error messages. mutsu now implements it with upstream's algorithm, step for step:

- `:max` defaults to 72 and `:indent` prefixes every line.
- SGR colour escapes do not count towards the width.
- A word wider than the limit becomes a line of its own.

This is a slice of ADR-11203 (#11208).
