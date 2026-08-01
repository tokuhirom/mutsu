# A Range index to `:exists` is read as a hash key, not a slice

Every other slice index form reaches `:exists` as a list of indices; a Range
does not, and answers a single `False`:

```raku
my @a; @a[0] = 1; @a[1] = 2;
say (@a[0,1]:exists).raku;    # raku: (Bool::True, Bool::True)  mutsu: same
say (@a[0..1]:exists).raku;   # raku: (Bool::True, Bool::True)  mutsu: Bool::False

my %h = "0" => 1, "1" => 2;
say (%h{0..1}:exists).raku;   # raku: (Bool::True, Bool::True)  mutsu: Bool::False
```

This is independent of shape — it is wrong for a plain `my @a` too. The value
adverbs get it right (`@a[0..1]:v` works), because
`builtins_multidim_subscript.rs` expands a Range index via
`nested_index_elements`, whose last arm is `_ if idx.is_range()`.

`exec_exists_index_adv_op` (`src/vm/vm_var_exists_ops.rs`) has no such arm. Its
`match idx.view()` handles `Int`, `Sub` (WhateverCode), `Array`, `Whatever` and
`+Inf`, and a Range falls through to the `_` tail, which is the *single-key*
path: it stringifies the index and looks it up as one hash key ("0..1"), so the
answer is one `False`.

The array half is a small arm (expand the Range to indices, as
`nested_index_elements` does). The hash half is the reason this was left out of
the shaped-slice fix: a Range subscript on a Hash is also a slice, but of
*keys*, and the `_` tail that would have to be split handles Hash, Set, Bag,
Mix, Stash and Instance targets in one `match (target.view(), idx.view())`. So
the fix is either two arms with different expansions, or hoisting the
index-expansion above the target dispatch for every target kind.

Found while fixing the shaped-array slice `:exists` collapse
([news](../../news/2026-08/shaped-array-slice-exists.md)).
