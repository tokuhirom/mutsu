# `$scalar[0]:exists` follows raku's one-element-list rule

A non-Positional value behaves as a one-element list holding itself under a
positional subscript — `5[0]` is `5` — so index 0 exists and nothing else does.
mutsu answered `False` for every index:

```raku
my $i = 5;      say $i[0]:exists;    # raku: True,   mutsu: False
my $s = "ab";   say $s[0]:exists;    # raku: True,   mutsu: False
my $r = <1/2>;  say $r[0]:exists;    # raku: True,   mutsu: False
                say $r[1]:exists;    # raku: False,  mutsu: False (agreed)
```

## Root cause

The exists opcode (`exec_exists_index_adv_op`) walks a chain of container arms —
Hash, Pair, Stash, Set, Bag, Mix, Instance, Mixin — and then falls through to a
generic tail that reads the target's array backing. For anything that is not an
array that backing is empty, so every index is out of range and the answer is
`False`. A bare `Int`/`Str`/`Rat` reaches exactly that tail. The read side was
already right (`$i[0]` returns 5); only `:exists` disagreed.

## Fix

A new `Value::is_one_element_scalar` names the plain scalar leaves — the numeric
types, `Str`, `Bool`, `Pair`, `Enum`, `Regex`, `Sub`/`Routine`, `Version` — and
excludes everything that is Positional, Associative, or carries its own
subscript protocol. The exists opcode routes those through the same
`instance_exists_pos_result` helper the `Instance` and `Mixin` arms use, and
`native_method_1arg` grows the matching `EXISTS-POS` (raku's `Any.EXISTS-POS`),
so `5.EXISTS-POS(0)` answers directly too. Restricting the native method to the
scalar leaves means a class declaring its own `EXISTS-POS` is never shadowed by
it.

Routing through the shared helper rather than special-casing index 0 is what
makes the rest fall out: slices (`$i[0,1]:exists` → `(True, False)`), the zen
slice (`$i[*]:exists` → `(True,)`), `:!exists`, and an *associative* subscript
(`5<a>:exists` → `False`, because no `EXISTS-KEY` is found on a scalar). An
undefined value stays the empty list — `my $n; $n[0]:exists` and
`Int[0]:exists` are both `False` — because the type objects are not scalar
leaves.

One general divergence surfaced on the way and is fixed here: a WhateverCode
index was never resolved by `:exists`, so `@a[*-1]:exists` answered `False` for
*every* array. Both paths apply it to the subscripted value's `.elems` now.

Pinned by `t/scalar-positional-exists.t` (21 assertions, verified to pass under
`raku` as well as mutsu).

## Not covered

Two neighbours are recorded as tickets, both blocked on the same missing piece —
the opcode does not carry the subscript kind, so it cannot tell `$c[0]` from
`$c{0}`: `todo/tickets/hash-in-scalar-positional-subscript.md` (a hash, set, bag
or mix in a scalar answers `[0]:exists` as a key lookup) and
`todo/tickets/scalar-subscript-value-adverbs.md` (`:kv`/`:p`/`:k`/`:v` on a
scalar subscript return Nil — a different opcode, which would need an
index-shape heuristic to stay safe for `5<a>:v`).
