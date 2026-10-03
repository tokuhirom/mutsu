# `.&chomp` writes through the topic; `()` beats `*@a is raw`

P5chomp exports Perl-style `chomp`/`chop` as multis. Two of their shapes did
not work under mutsu.

`$_ = "b\n"; .&chomp` calls `multi chomp(\s) { s .= chomp }`. A sigilless
parameter binds the caller's container, but a code-object call (`.&g`,
`$x.&g`, `&g($x)`, `$code($x)`) never tagged its variable argument the way
an ordinary `g($x)` call does. `$x.&g` happened to recover the container by
name, but the topic had no such route and died with "Cannot modify an
immutable value". Those calls now tag a plain variable argument with its
container too.

`chomp()` with no arguments must reach `multi chomp() { die ... }`. Instead
it went to `multi chomp(*@a is raw)`, because the candidate ranking counted
the slurpy's `is raw` as "needs a writable argument", which made the slurpy
look narrower than the empty signature. Only a non-slurpy `is rw` / `is raw`
parameter counts now.

All three of P5chomp's test files pass. Writing through a sigilless
parameter bound to a `for` loop's topic is tracked as #11447.
