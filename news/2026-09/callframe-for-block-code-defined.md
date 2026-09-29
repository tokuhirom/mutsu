# `callframe` inside a `for` block reports a defined `Block` code object

The synthetic call frame for an enclosing `for` block used the `Block` type object as its
`.code`, so `for ^1 { $f = callframe }; $f.code.defined` was `False` where Rakudo says `True`.
The frame now carries a defined, empty `Block` (still `.^name` `Block`, not a `Routine`), with no
routine name/package/subtype. Pinned by `t/vm/frames/callframe-for-block-code-defined.t`.
