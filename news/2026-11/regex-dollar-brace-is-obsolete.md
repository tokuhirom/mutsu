# `${name}` in a regex is refused as rakudo refuses it

`${name}` is Perl 5's braced scalar. In a Raku regex rakudo refuses it at
compile time with `X::Obsolete`: "Unsupported use of ${x}. In Raku please use:
$x.", or `{$x}` inside a `"..."` literal. mutsu read it as `$x` and spliced the
value, so `"xa" ~~ / x ${x} /` matched (#18925).

The parse-time structural regex validator now raises that `X::Obsolete` for a
braced identifier. It skips a `'...'` literal, a backslashed `\$`, the `$$`
anchor (also before a code block) and `${ <expression> }`. A pattern built at
run time (`<$str>`) is refused the same way, in place of the interpolation
pre-pass's braced splice.
