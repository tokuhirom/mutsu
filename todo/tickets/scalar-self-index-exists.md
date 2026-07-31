# `$scalar[0]:exists` is False where raku says True

A non-Positional scalar behaves as a one-element list under a positional
subscript, so `$x[0]` is `$x` and index 0 exists. mutsu answers `False` for every
index:

```raku
my $i = 5;      say $i[0]:exists;    # raku: True,   mutsu: False
my $s = "ab";   say $s[0]:exists;    # raku: True,   mutsu: False
my $r = <1/2>;  say $r[0]:exists;    # raku: True,   mutsu: False
                say $r[1]:exists;    # raku: False,  mutsu: False (agrees)
```

The read side already does the right thing (`$i[0]` returns 5); only the
`:exists` adverb disagrees. In `exec_exists_index_adv_op`
(`src/vm/vm_var_exists_ops.rs`) the chain of container arms — Hash, Pair, Stash,
Set, Bag, Mix, Instance, Mixin — ends in a generic tail whose last arm is
`_ => false`, and a bare scalar reaches it.

Found while fixing `:exists` on a mixin
([news](../../news/2026-07/exists-adverb-dispatches-through-a-mixin.md)); it is
not mixin-specific (a plain `Int`/`Str` shows it too) and predates that change.
Presumably the fix is a final arm treating a non-Positional, non-Associative
target as a one-element list: index 0 (and -1) exist, everything else does not.
Check what `:kv`/`:p` and a slice (`$i[0,1]:exists`) should return before
settling on the shape.
