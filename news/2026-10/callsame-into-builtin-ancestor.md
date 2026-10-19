# `callsame` from a subclass of a built-in type reaches the built-in method

A user method on a subclass of a built-in type that defers with
`callsame`/`nextsame` now reaches the built-in method of the same name. In
Rakudo that method is a real candidate of the receiver's MRO. In mutsu the
deferral frame added a built-in base candidate only for an enumerated set of
cases:

- an `augment` of a core type;
- `gist`/`Str`/`raku`;
- container storage;
- `Mu`'s constructors;
- metamodel HOWs;
- grammar rules.

So these all answered `Nil`:

- `class S is Str { method uc { '[' ~ callsame() ~ ']' } }`
- `class I is Int { method succ { nextsame } }`
- `class P is Pair { method key { 'k:' ~ callsame } }`
- `class C is Cool { method uc { 'c' ~ callsame } }`

A new base, `NativeBase::BuiltinAncestor`, covers every receiver whose MRO
reaches a built-in method of that name. It runs the built-in on the receiver
with the override hidden, through the same bypass an `augment` uses. An
`is List` subclass also reads through its backing storage the way an
`is Array` one does, so `class L is List { method elems { 10 + callsame } }`
counts its elements. This is part of slice 4 of the method table campaign
(ADR-11276).
