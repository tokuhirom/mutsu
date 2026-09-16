# Acme::Insult::Lala: itemized-Seq destructuring and classify's WhateverCode topic

`Acme::Insult::Lala`'s `TWEAK` builds its word lists with
`%?RESOURCES<lala.txt>.lines>>.split(/\s+/).map(-> [$a, $b, $c] { a => $a,
b => $b, c => $c }).flat.classify(*.key, as => *.value)`. Chasing it down
found two separate general gaps, both regression-pinned under `t/`.

## Sub-signature destructuring didn't see through `$`-itemization

An array/hash *element* read wraps a value that has no `ArrayKind`-style
itemized flag of its own (a `Seq` in particular) in a `Value::Scalar` box to
mark it itemized. `positional_values_from_unpack_target`
(`runtime/types/signature.rs`) already unpacked an itemized `Array` correctly
— itemization there is a flag on the `Array` view itself — but stopped at the
`Value::Scalar` box for everything else, reporting "Too few positional
arguments in sub-signature binding" for a value that plainly had three
elements: `@a[0] = "x y z".split(/\s+/); -> [$a,$b,$c] {...}(@a[0])` died in
mutsu while rakudo destructures it fine, itemization notwithstanding. The fix
descalarizes before the unpack, mirroring the itemized-`Array` case.

Pinned by `t/routines/signature/subsig-destructure-through-itemization.t`.

## `.classify`'s WhateverCode mapper never saw `$_`

A bare WhateverCode mapper (`*.key`) compiles to a one-param closure whose
param is literally named `"_"`. `legacy_has_plain_positional_param`
deliberately excludes `"_"` from ordinary positional binding for a
Pair/ValuePair-shaped argument — that shape's implicit argument is meant to
arrive through the dynamically-scoped topic, the same way `.map`/`.grep`
already topicalize their per-element callback (`vm_call_map_block`'s
`explicit_topic`). `builtin_classify` called its mapper (and its `:as`
mapper) through plain `call_sub_value` with no such topicalization, so
`*.key` over a Pair/ValuePair element saw an `Any` invocant and `.key` failed
with "No such method 'key' for invocant of type 'Any'". `builtin_classify`
now topicalizes `$_`/`_` around both calls.

Pinned by `t/collections/listop-classify-whatevercode-topic.t`.

Both fixes turned `Acme::Insult::Lala` fully green (2/2 baseline files).
