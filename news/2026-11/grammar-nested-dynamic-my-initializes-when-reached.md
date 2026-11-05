# A `:my $*x = ...;` inside a group of a rule runs when the group is reached

A grammar rule's frame ran every `:my $*x = ...;` initializer of the rule at its
entry, wherever the declaration sat. One inside a group, such as
`[ ';' :my $*negative = True; <pattern> ]?`, therefore set the variable even
when the group never matched, and Intl::Format::Number's number pattern (guarded
by `<?{$*negative}>`) took its negative branch for every pattern: its methods
never ran and `t/04-grammar.rakutest` was 6 of 130 (#16307).

The scan of a rule's dynamic declarations now notes how deeply a declaration is
nested (`pattern_group_depth_at`). A nested one is bound at entry as the
uninitialised `Any` (so the rule and the rules it calls find the variable, and
the frame restores it on exit) and its `VarDecl` atom runs the initializer when
the group is reached, installing the value for the rest of the rule and its
sub-rules. A top-level declaration is unchanged. `t/04-grammar.rakutest` now
passes 130 of 130 like rakudo. Pinned by
`t/grammar/grammar-nested-dynamic-my-initializes-when-reached.t`.

Not done: the same name declared at the top level of one rule and inside a group
of another is treated as deferred in both (the atom cannot tell the rules
apart), which only matters for an initializer with side effects.
