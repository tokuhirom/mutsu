# `:=`-bind ancestor-frame propagation clobbers same-named locals across unrelated recursive invocations

Discovered while resolving
`todo/tickets/bind-alias-saved-locals-wrong-frame-index.md` (now
`news/2026-08/bind-alias-saved-locals-dead-code-removed.md`). That ticket was
scoped narrowly to the `saved_locals[i]` half of
`Interpreter::propagate_bind_to_ancestor_frames`
(`src/vm/vm_var_assign_ops.rs`) and confirmed it is dead code — but the
investigation surfaced a separate, pre-existing bug in the OTHER half of the
same function (the `frame.saved_env` splice), which is explicitly out of that
ticket's scope and is filed here instead.

## The bug

`propagate_bind_to_ancestor_frames(name, code, container)` walks
`self.call_frames` in reverse and, for every ancestor frame whose
`saved_env` has `name` in its own tier
(`frame.saved_env.contains_key_own_tier(name)`), splices `container` into
that frame's `saved_env`. This check is purely name-based: it does not
distinguish "an ancestor frame that is the true declaring scope of a
captured free variable" from "an unrelated ancestor invocation of the *same
recursive function* that happens to have declared its own separate local
with the same name."

For an ordinary closure-capture bind (the case this mechanism exists for,
covered by `t/bind-source-tracks-through-call-chain.t`) there is exactly one
true declaring scope on the stack, so the name-based match is harmless. But
for a **recursive function that re-declares a same-named local at every
call depth**, every ancestor invocation's `saved_env` legitimately owns that
name in its own right — as a totally independent lexical, per Raku's normal
scoping. The loop still splices the SAME shared container into every one of
them, silently aliasing what should be N separate lexicals into one shared
cell.

### Repro

```raku
my @levels;
sub rec(Int $n) {
    my $v = $n;
    if $n > 0 {
        rec($n - 1);
    } else {
        my $x := $v;
        $x = 999;
    }
    @levels.push($v);
}
rec(3);
say @levels;
```

- `raku`: `[999 1 2 3]` — only the base case's own `$v` is affected; `rec(1)`,
  `rec(2)`, `rec(3)` each keep their own untouched `$v`.
- `mutsu` (current, both with and without the `saved_locals[i]` patch that
  the sibling ticket removed): `[999 999 999 999]` — every recursion level's
  `$v` gets silently aliased to the base case's bound cell.

Confirmed the `saved_locals[i]` patch is not the cause (nor a fix): the
output is byte-identical with that loop body deleted, which is exactly the
evidence the sibling ticket used to justify removing it. This bug lives
entirely in the `frame.saved_env.contains_key_own_tier(name)` match /
`frame.saved_env.insert(...)` splice, unconditionally on every ancestor
frame that matches by name.

## Why this is filed separately (not fixed alongside the sibling ticket)

The sibling ticket explicitly scoped itself to the `saved_locals` half only,
declaring the `saved_env` splice "the real, working general-case mechanism"
to be preserved as-is; this bug is inside that supposedly-working mechanism.
Fixing it needs its own investigation: the splice would need to distinguish
"the true single declaring frame for a captured free variable" from
"a same-named local re-declared independently at another call depth" —
plausibly by keying on frame identity/depth (e.g. only splice into the frame
that was live when the *variable itself* — not just its name — was captured/
declared), which likely needs either a per-frame declaration-depth tag or a
genuine identity token for the bind's source, not a bare name string. That is
architecturally bigger than a follow-up slice of the sibling ticket.

## Affected files

- `src/vm/vm_var_assign_ops.rs` — `propagate_bind_to_ancestor_frames`
- Call sites: `src/vm/vm_var_assign_set_local.rs` (two branches),
  `src/vm/vm_exec_dispatch.rs` (`SetGlobal` bind handler)

## Suggested next step

Reproduce with the repro above (now also captured, in a form that pins
*mutsu's current, still-buggy* output, as
`t/bind-alias-recursive-frame-index.t` from the sibling ticket's PR — that
test intentionally does NOT assert the raku-correct `[999 1 2 3]` result; it
pins current behavior so this ticket's eventual fix has a test to flip green).
Investigate whether call-frame identity (not just name) can gate the splice,
or whether the propagate mechanism needs to track the originating frame of
the *bind's source variable* explicitly rather than rediscovering it by name
at every ancestor.
