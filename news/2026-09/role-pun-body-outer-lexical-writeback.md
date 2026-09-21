# A role body's write to an outer lexical now survives punning

A role body is allowed to do arbitrary work, including writing to a lexical
declared outside the role. Rakudo runs that body when the role is composed
— punning (calling a method straight on the role type object) counts — and
the write reaches the enclosing scope afterwards:

```raku
my $side;
role Base { $side = "BODY RAN"; method kv() { "method" } }
say Base.kv;
say $side;   # BODY RAN
```

mutsu already ran the body (fixed earlier for `Hash::Ordered`/`Hash::Agnostic`
support), but the write to `$side` never reached the enclosing scope.

The root cause was in the shared writeback mechanism `run_compiled_block_raw`
uses to carry a deferred class/role body's outer-lexical writes back to the
frame that declared them. It recorded those writes in
`pending_rw_writeback_sources`, a drop-on-miss, single-frame list meant to be
drained by a call site one hop away — which is exactly what happens for a
decl-time `does` composition (the `RegisterClass` opcode drains it
immediately after the body runs).

Punning triggered from inside method dispatch (`ensure_role_punned_to_class`,
reached from a bare `Role.method` call deep inside `call_method_with_values`)
has no such adjacent call site: the actual method dispatch that follows
composition is itself an intervening call, and its `call_compiled_method`
unconditionally clears `pending_rw_writeback_sources` at entry — silently
dropping the pun body's write before the frame that owns `$side` ever gets a
chance to drain it.

The fix records these writes with `record_caller_var_writeback` instead, the
retain-on-miss list already used for a `where`-clause or `EVAL`'d snippet's
caller-lexical writes for exactly this reason: the owning frame may be
reached only after further intervening calls. It is a superset of the
previous behavior — the decl-time case still drains in one hop — and
additionally survives punning's extra dispatch hop.

Pinned by two new assertions in
`t/oo/role/pun-composes-role-body-declarations.t`.

Closes [#8862](https://github.com/tokuhirom/mutsu/issues/8862).
