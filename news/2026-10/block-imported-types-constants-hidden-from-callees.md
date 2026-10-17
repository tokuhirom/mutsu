# A block's imported classes and constants are hidden from a callee's string lookups

A top-level routine that looks a name up by string (`::('Foo')`,
`EVAL 'KONST'`) resolves it in its own lexical outer. When a block calls such
a routine after `use M`, the block-scoped import is not in that outer. Since
#13604 the routine's frame hid the variables the open block imports bound.
It now also hides their constants and type names:

```raku
sub callee { (try ::('Foo').hi) // 'no Foo' }
{ use M; say callee() }   # rakudo: no Foo; mutsu found M's class Foo
```

The filter that picked the hidden names admitted only lowercase,
variable-shaped keys, so an uppercase `constant KONST` or a `class Foo` stayed
visible. A symbol of the same name in the routine's own outer is the value
the import shadowed, so it is still the routine's answer (ADR-8751).
