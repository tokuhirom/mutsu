# A `|c` capture keeps the caller's container of a named argument

`sub via(|c) { w |c }; via :p($a)` handed `w` the value of `$a`, not the
variable, so an `is raw` / `is rw` named parameter died with "Cannot assign to
an immutable value" (or, in a `multi` pair with a copy candidate, silently wrote
nothing). Positional arguments already kept their container (#11295); a named
argument now does too, with its writeback keyed by the argument name.

Two more pieces were needed for Deps' `deps-root :deps(my $root), { ... }`,
which is exported as `-> |c { deps-root |c, :class(Deps) }`: an inline
`:key(my $x)` argument now takes a real lexical slot and is tagged like
`:key($x)`, and an undefined caller variable is not mistaken for a bare type
name. Deps 0.0.1 `t/01-basic.rakutest` is 74/74, as on rakudo (#16121).
