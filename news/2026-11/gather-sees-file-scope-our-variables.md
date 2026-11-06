# A `gather` body sees the file-scope `our` variables its routines name

A plain `our $x` / `our @a` / `our %h` at file scope is reachable under its bare
name only through the env of the scope that declared it. A `gather` keeps just
its body's own free variables, so a routine called from the body read `Any`:

```raku
our $a = 'A';
sub f { $a }
say (gather { take f() })[0];   # (Any) before, A now
```

`MakeGather` now hands the file-scope `our` containers (kept in
`LexicalState::global_our_names`, filled by `set_our_var`) to the captured env
unless the body already captured a nearer lexical of that name. Reads, `++`,
`+=` and plain assignment inside the gather all reach the declaring scope's
variable.

Found through Math::Sequences, whose `@A002904` (`lazy gather` over
`from_roman`, which reads `Math::Util::Roman`'s `our $rom` / `our @val`) hung
`t/numberphile.rakutest`. Pinned by `t/collections/lazy-seq/gather-file-scope-our-variable.t`.
