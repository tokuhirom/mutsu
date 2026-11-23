# `${name}` in a regex is rejected as obsolete, as on rakudo

`/ x ${x} /` is Perl 5 syntax. rakudo rejects it at compile time with
`X::Obsolete`: "Unsupported use of ${x}. In Raku please use: $x." Inside a
`"..."` literal the suggested replacement is `{$x}` instead.

mutsu used to read `${x}` as `$x` and splice its value into the pattern. That
was the last place the interpolation pre-pass still spliced a bound lexical
(#14510). Now:

- the parse-time regex validation raises the same `X::Obsolete` as rakudo;
- a pattern built at run time, such as a `<$var>` source, is rejected the
  same way;
- `${ code }` with a space after the brace is still the end anchor followed by
  a code block (#18925).
