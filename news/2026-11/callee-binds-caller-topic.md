# A callee's bind of its caller's `$_` is seen by the caller

`nqp::bindlexdyn('$_', $v)` and `CALLER::<$_> := $v` bind the *calling* frame's
`$_` (likewise `$/` and `$!`). In rakudo the caller reads the new value once
the call returns:

```raku
use nqp;
sub g($v) { nqp::bindlexdyn('$_', $v) }
sub f { $_ = 0; g(5); say $_ }    # 5
```

mutsu printed `0` (#17453). A frame's topic lives in its env, and every return
path restores the caller's env from the copy the call saved. The bind wrote
into a snapshot that nothing read back. The bind now writes into the saved
copy itself, so whichever return path runs hands the caller its new binding.
The callee's own topic is untouched, and the return writeback still never
copies a callee's own `$_` back.

`CALLER::<$_> := $v` (and `$/`, `$!`) now compiles to the pseudo-stash bind
and takes the same path. Reading or assigning `CALLER::<$_>` should die as rakudo
does ("not declared as dynamic"); that difference is tracked separately.
