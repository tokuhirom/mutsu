# `&f` mentions of a sub that writes a free variable share one identity

`&f` got the stable registration id only when the compiled body wrote no free variable, because
a stable id used to leak a stale value through the per-id persisted free-variable state
(`closure_captured_state`). ADR-0055 slice 5 retired that store, so the restriction is gone:
`my $n = 0; sub f { $n++ }; say &f.WHICH eq &f.WHICH` is `True` as in raku, and a `.wrap` keyed on
one mention is visible through the others. `roast/S03-metaops/hyper.t` and
`t/lang/code-var-mention-identity-stable.t` pin that a reassignment of the captured variable
between mentions is still seen (#14573, part 1).
