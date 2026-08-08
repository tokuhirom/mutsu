# A loop body's `my` no longer outlives its block

`pop_loop_local_scope`'s doc comment promised to "restore the env entries for
loop-body-local `my` names to the values they shadowed before the loop (or
remove names that did not exist before)". The parenthesis was never implemented:
`loop_local_saved_env` is a `HashMap<String, Value>`, and `exec_set_var_dynamic_op`
only recorded an entry when the declaration found an existing binding to shadow.
A body-local `my` with no enclosing namesake therefore had nothing recorded, and
its last-iteration value stayed in `env` under its bare name for the rest of the
program:

```raku
for 1..1 { my int $i = 3; while --$i >= 0 { } }
say ::('$i');    # raku: Nil;  mutsu: -1
```

The map now carries `Option<Value>`: `Some(v)` is the pre-existing shadow to
re-expose, `None` a removal marker for a name the loop invented. Scope exit
removes those instead of leaving them behind, and skips the local-slot
write-through for them (there is no outer slot to restore).

`state` declarations are excluded outright. A `state` in a loop body is
re-executed every iteration but denotes ONE binding that accumulates across them
and must survive the loop — zef's `@*ARGS` reordering
(`for @*ARGS -> $arg { state @named; state @positional; LAST { @*ARGS = flat @named, @positional } … }`)
is exactly that shape, and sweeping those away emptied `@*ARGS`, which broke
every `zef` invocation down to `mzef --version`. That case is now pinned too.

Found while reducing why Cro's `t/http-session-inmemory.rakutest` reported
"request -1" for every request: `HTTP::HPACK`'s Huffman-table builder leaves a
block-local `my int $i` at `-1`, which reached the test's own `for 1..5 -> $i`
loop variable. This closes the `env` half of that leak; the value is still
reachable through another store, tracked in
`todo/deep/hpack-module-body-lexical-leaks-into-an-unrelated-frame.md`.

Pinned by `t/loop-body-my-does-not-outlive-the-block.t`.
