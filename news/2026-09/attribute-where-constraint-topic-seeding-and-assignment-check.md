# Attribute `where` constraints: fixed topic seeding, and enforced on later assignment

`has Numeric $.lat where { -90 <= $_ <= 90 }`-shaped attribute declarations had two
compounding bugs, found while working the ecosystem `Date::Event` distribution's
`t/5-lat-lon.t` (locked on [tokuhirom/mutsu#8977](https://github.com/tokuhirom/mutsu/issues/8977)).

**Root cause (predates this change, affected construction too):**
`check_attribute_where_constraint` only seeded the implicit topic (`$_`) before
evaluating the predicate when a compiled chunk's `free_var_syms` happened to
record a read of `_`. `$_` is a magic/dynamic variable resolved through `env`,
not a genuine lexical closure capture, so an ordinary block predicate that reads
it directly (`where { -90 <= $_ <= 90 }`) did not reliably show up in
`free_var_syms`. `$_` then stayed unseeded, the block evaluated against a
stale/absent topic, and the predicate silently always evaluated true regardless
of the candidate value — confirmed with `Foo.new(lat => 999)` already silently
succeeding on `main` before this fix, for a plain `has $.x where { $_ > 0 }`.

Fixed by always seeding `$_` before evaluating the predicate, mirroring the
subset `where`-predicate's own unconditional inline execution
(`type_matches_value`'s `Expr::Block`/`Expr::Lambda` handling) instead of trying
to detect whether the predicate "uses" the topic first. This is safe for every
predicate shape: a plain value/type/Junction predicate never reads `$_`, so
seeding it is a no-op, and `smart_match_values` already implements "a Bool RHS
is the match result regardless of the topic" (rakudo: smartmatch against `True`
always matches) — so a block/`.so` predicate's boolean result and a plain
value/type/Junction predicate both resolve correctly through the same
`smart_match_values` call.

**Second gap: assignment-time enforcement was entirely missing.** Even with the
topic bug fixed, an attribute's `where` constraint was only ever checked by
`enforce_attribute_where_constraints` at `.new()` construction time. A later
`$!lat = $v` inside a method (`Date::Event`'s `lat`/`lon` mutator accessors)
reached the plain scalar-assignment paths, which only ever consulted the
attribute's declared TYPE (`scalar_attr_type_constraint`) and silently accepted
any value that merely satisfied that type. Added `self_attr_where_constraint`
(mirroring `self_attr_type_constraint`'s MRO walk, gated behind a
process-global "ever declared" flag so the registry walk is skipped entirely
for the overwhelming majority of programs that never declare an attribute
`where` clause) and wired a new `check_scalar_attr_where_on_assign` call into
the three plain scalar-assignment checkpoints
(`exec_set_local_op_inner`, `exec_assign_expr_local_op_inner`, and the
name-based `AssignExpr` path in `vm_misc_assign.rs`), right after their
existing type-constraint handling.

`Date::Event` 0.0.12 moved from `partial` (4/5 baseline files, 103/107
assertions) to `green` (5/5 files, 107/107 assertions). Pinned by
`t/oo/attribute/attribute-where-block-rejects-out-of-range.t`.
