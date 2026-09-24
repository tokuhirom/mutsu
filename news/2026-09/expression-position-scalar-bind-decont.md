# Expression-position `(my $p := EXPR)` no longer itemizes its value

`List::MoreUtils`' `natatime` returns chunks through a closure whose body is
`(my $pulled := $iterator.pull-one) =:= IterationEnd ?? () !! $pulled`. With a
chunk size of 1, `while $it() -> @vals { @out.append(@vals) }` produced
`[(1,), (2,), (3,)]` instead of rakudo's `[1, 2, 3]` (#9262).

The root cause was not in `rotor` or in Code invocation at all: a scalar `:=`
bind written in *expression* position lost its "bound, not a Scalar container"
status, so a later `@a = $p` itemized the List instead of flattening it. The
statement form (`my $p := (1,2); my @w = $p`) was already correct.

Two paths were missing the signal the statement form sends:

- `expr_block.rs` compiled the declaration's store without the
  `MarkScalarBindContext` the statement-form `SetLocal` gets, so the store
  itemized the Positional and recorded no bound-decont marker (the closure /
  routine case, where the name has a local slot).
- `SetGlobal` (the mainline case, where an expression-position declaration has
  no local slot) consumed the scalar-bind flag but never called
  `update_bound_decont_marker`, so `ItemizeVar` could not see the bind.

Both now mirror `SetLocal`. Pinned by `t/collections/expr-scalar-bind-flattens.t`.
