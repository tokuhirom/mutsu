# A closure reads its block's `my class` after the block exits

A closure written inside a block that declares `my class Inner` (or
`my role`) used to resolve the bare name `Inner` in whatever scope it ran in.
Called after the block had exited, it found a same-named outer class instead:

```raku
class Inner { method tag { "outer" } }
sub a() { my $c; { my class Inner { method tag { "block" } }; $c = { Inner.tag } }; $c() }
say a();   # was "outer", now "block" as in Rakudo
```

The compiler now records the lexical types each block declares, and a closure
body nested in such a block reads the name through the type's own binding as a
by-name read, which the closure captures when it is created, the way it already
captures an enclosing `my \x` (#17711).
