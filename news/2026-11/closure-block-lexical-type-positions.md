# A closure's type positions name a block's `my class` after the block exits

```raku
class Inner { method tag { "outer" } }
sub a() { my $c; { my class Inner { method tag { "block" } }; $c = { my Inner $x = Inner.new; $x.tag } }; $c() }
say a();   # was "Type 'Inner' is not declared", now block as in Rakudo
```

A closure that runs after its block exited already read the block's lexical
type through the type-only key it captured, but only as a bare term. A
variable's type constraint, a parameter type and a `does` trait resolved the
name through the bare env binding or the registry instead, which found the
same-named package type (or nothing), so a parameter accepted the wrong class
and an anonymous `class :: does R` composed both roles. Those positions now
consult the lexical type's type-only binding first, so they name the block's
type as the closure's bare terms do (#18177).
