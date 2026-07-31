# `:kv` / `:p` / `:k` / `:v` on a scalar subscript return Nil

`$scalar[0]:exists` now follows raku's one-element-list rule
([news](../../news/2026-07/scalar-positional-exists.md)); the value-returning
adverbs of the same subscript do not:

```raku
my $i = 5;
say ($i[0]:kv).raku;   # raku: (0, 5)      mutsu: Nil
say ($i[1]:kv).raku;   # raku: ()          mutsu: Nil
say ($i[0]:p).raku;    # raku: 0 => 5      mutsu: Nil
say ($i[0]:k).raku;    # raku: 0           mutsu: Nil
say ($i[0]:v).raku;    # raku: 5           mutsu: Nil
```

These are a different opcode: `:exists` compiles to the exists opcode
(`exec_exists_index_adv_op`, `src/vm/vm_var_exists_ops.rs`), while `:kv`/`:p`/
`:k`/`:v` compile to a `__mutsu_subscript_adverb` call handled by
`builtin_subscript_adverb` (`src/runtime/builtins_multidim_subscript.rs`). That
function already coerces a non-Array, non-Hash Positional target (Range, Seq,
LazyList) to a plain array up-front so the Array arm owns the value/key logic;
the natural fix is to coerce a scalar to a *one-element* array in the same
place, using `Value::is_one_element_scalar`.

The catch, and why it was not folded into the `:exists` slice: the coercion must
not apply to an *associative* subscript. raku answers `5<a>:v` with `()`, but the
opcode does not carry the subscript kind, so a coerced `[5]` array with the Str
index `"a"` would coerce that key to index 0 and wrongly return `5`. So the
coercion needs a positional-index test (Int / Whatever / `Inf` / WhateverCode /
Range / a list of those) that the `:exists` path gets for free — there, a Str
index simply finds no `EXISTS-KEY` on a scalar and answers False.

Also worth pinning while doing it: raku *throws* on an out-of-range scalar slice
for the value adverbs (`5[0,1]:v` dies with "Index out of range. Is: 1, should be
in 0..0") where `:exists` answers `(True, False)`.
