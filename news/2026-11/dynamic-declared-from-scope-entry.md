# A scope's `my $*x` is bound from the scope's entry

Rakudo keeps a block's lexicals in its pad, which exists from the block's
entry. So a dynamic that a block declares further down already shadows the
callers' binding before its declaration runs, and reads as the declaration's
unset value (`Any` for an untyped scalar) until then. A dynamic declared in a
nested block that has not been entered shadows nothing.

mutsu got both halves wrong, in different places. A plain `$*x` read from a
routine called before the declaration found the caller's binding instead of
the pending one. `DYNAMIC::<$*x>` went the other way: it treated any frame
whose code declared the name anywhere, including a dormant nested block, as
the owner, and answered `Any`. Red's `red-do` proto declares
`$*RED-ROW-CONTEXT` in a nested block and reads `DYNAMIC::<$*RED-ROW-CONTEXT>`
before it, so a nested `red-do` lost the outer row-security context (#19174).

Now the compiler declares, at each scope's entry, every `my $*x` of the
scope's statement list that some code could run before, without its
initializer. The declaration in source order binds it again with its
initializer. `DYNAMIC::<$*x>` resolves through the same env chain a `$*x`
read uses, and the frame walk over `CompiledCode.locals` is gone for dynamic
keys. Red 0.2.17's `t/193-row-security` and `t/198-row-security-dynamic-context`
now pass as they do on rakudo.
