# A closure over a `for` parameter was hijacked by a later same-named `my`

Found while working
`todo/tickets/for-kv-multi-param-bind-decontainerizes.md`: the `.kv` fix it
prescribes emits a `my $v := ...` binding, which turned out to break unrelated,
already-passing ADR-0045 rows. The trigger was not the new binding — it was a
pre-existing bug that any same-named `my` exposes.

## The bug

A closure created inside a `for` loop captures the loop's **parameter**. That
capture is resolved at run time by `resolve_capture_slot`
(`src/vm/vm_register_ops.rs`), whose fallback is a name search over the creating
frame's local slots:

```rust
sym.with_str(|s| code.locals.iter().rposition(|n| n == s))
```

A frame's local slots are flat — there is no per-block scope and no record of
*where* in the frame a slot was declared — so the search happily found a slot
declared **later** in the same compilation unit. A sibling block's `my $v` is a
different lexical entirely, but it shares the name:

```raku
{
    my @a = 10, 20;
    my @c;
    for @a -> $v is rw { @c.push(-> { $v = $v + 1 }) }
    @c[0](); @c[1]();
    say @a;          # raku [11 21]   mutsu [10 20]
}
{
    my $v = 1;       # <- deleting this block makes the one above pass
    say $v;
}
```

The read-only form diverges the same way (`-> { $v }` yielded `Nil Nil`). Both
blocks in isolation were correct, which is why it went unnoticed: it takes two
sibling scopes reusing one name, in one file, to show.

`CompiledCode::for_loop_param_syms` already exists for exactly this hazard — a
`for` parameter is the loop's own binding, never a slot to capture from the
enclosing frame — but it was applied only to the compile-time free-variable
analysis, not to this runtime resolution.

## Fix

`resolve_capture_slot` declines the name search when **both** hold:

* the symbol is a `for` loop parameter of the running frame
  (`code.for_loop_param_syms`), and
* the emit-time bake (`free_var_parent_slots`, the creating frame's `local_map`
  *at the closure's creation point*) says `None` — the frame had no such local
  then.

The bake supplies exactly the position information the name search lacks, so
the test is precise rather than heuristic: a genuine capture of a `my` declared
*before* the closure bakes `Some(slot)` and is untouched. Only the "there was no
such local yet" case changes, and there the correct answer is the loop's own
binding, reached through the name's env entry.

Pinned by `t/closure-capture-not-hijacked-by-later-my.t` (4 tests: the rw and
read-only closure forms, the sibling `my` that triggers it, and the
capture-of-an-earlier-`my` control).

Verified with `make test`, the full whitelisted roast sweep (1435 files,
218 833 tests) and the bundled-library gate (274/297, `GATE PASSED`).
