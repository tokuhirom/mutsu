# A finished `for` loop's `:=` alias no longer reaches a later loop's block exit

`for [1,2,3] -> $b { my $x := $b; ENTER { 1 }; ... }` followed by another loop
whose body has an `ENTER`/`FIRST` region read `$b` as the previous loop's last
value, because the exit sync of that region still offered the first loop's
`$x` slot (same alias key, declared earlier in the chunk) and wrote its stale
value over the target. A local declared in the body of a `for` loop that is
over by the time the block exits is now out of scope for that sync
(`CompiledCode::decl_scope_closed_at`). Finishes the residual of #15459.
