# A method's exit no longer undoes a `:=` rebind a nested call made

`my $p := $!cur` followed by a nested call that rebinds the attribute with `$!cur := 2`
left the attribute at its old value once the outer method returned, and
`ForwardIterables`' `pull-one` (`my $pulled := $!current.pull-one`, then a recursive pull
after `next-iterator` rebound `$!current`) read an exhausted iterator on the next call.
Two causes. `my $p := $!attr` boxed the method's seeded attribute slot into a cell
disconnected from the instance; it now aliases the instance's own container
(`AttrContainerRef`), as an `is rw` tail already does. And `reconcile_attrs` adopted any
cell-valued scalar `!x`/`.x` slot at method exit as a `:=` binding, though the frame also
boxes the slot for a mutating call or an `is rw` argument; a real `$!x := $y` seats its
cell when it runs, so only the aggregate and bare sigilless forms keep that recovery.
`ForwardIterables` 0.0.4 `t/01-basic.rakutest` is 7/7 like Rakudo. Fixes #14663.
