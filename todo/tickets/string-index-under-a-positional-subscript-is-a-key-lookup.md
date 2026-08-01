# A string index under a `[...]` subscript is still read as a key

A positional read of a value that does not do `Positional` now follows raku's
one-element rule ([news](../../news/2026-08/positional-read-of-a-non-positional.md)),
but only for numeric indices. A *string* index under the same bracket still
takes the associative route:

```raku
my $h = { a => 1 };
say $h["a"];   # raku: dies, X::Str::Numeric ("Cannot convert string to number")
               # mutsu: 1  -- read as the key "a"
```

raku coerces a positional index with `.Int`, so a non-numeric string index is a
coercion failure, not a key. The same rule governs an ordinary array
(`@a["1"]` is `@a[1]`, `@a["x"]` dies), so this is one rule with two call sites
rather than an Associative-specific quirk.

It is filed separately from the numeric fix because the blast radius is
different in kind. The numeric arms only changed answers that were already
wrong (a Set answering the membership of the key `0`); making `$h["a"]` die
turns a currently-working spelling into an exception, and mutsu's own bundled
modules may well use it — the batteries gate does not run on a PR that does not
touch `modules/`, so this one needs a local `scripts/battery-testsuite.sh` run
before it can be trusted.

The read path is the `(_, ValueView::Str(_))` family in
`Interpreter::exec_index_op_with_positional` (`src/vm/vm_var_index_ops.rs`);
`is_positional` is already in hand there, and `RuntimeError::str_numeric` (or
whatever `X::Str::Numeric` builder exists) supplies the exception. Check the
`(ValueView::Array(items, is_arr), ValueView::Str(s)) if is_positional` arm at
the same time — it should share the coercion rather than have its own.

A related divergence is worth confirming while in here, though it is probably
the general Failure model rather than this path: a slice that addresses an
out-of-range slot (`$h[0,1]`) answers a list whose second element is the
X::OutOfRange Failure in mutsu, while rakudo throws as soon as the list is
used. Both agree that `$h[1]` alone is a Failure.
