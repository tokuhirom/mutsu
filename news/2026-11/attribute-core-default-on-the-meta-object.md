# `trait_mod:<is>($attr, :default(...))` records the default on the attribute

A custom attribute trait that re-dispatches to CORE's `:default` candidate
(ASN::BER's `is default-value(...)`) now leaves the value on the attribute's
meta-object, as the `:rw` and `:built` candidates already did. The declaration
site reads all three back the same way and folds them into the attribute's
definition.

This removes `Interpreter::trait_mod_default_writeback`, a side channel that
the class body drained after each attribute's traits ran (ADR-10779 D3:
`Interpreter` 79 → 78 fields). Because the role-composition path folds the same
effects, the default now also applies to an attribute declared in a role:

```raku
multi trait_mod:<is>(Attribute $a, :$dv!) { trait_mod:<is>($a, :default($dv)) }
role R { has $.r is dv(7) }
class C does R {}
say C.new.r;   # 7
```
