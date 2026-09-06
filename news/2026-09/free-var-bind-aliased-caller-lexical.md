# A `:=` of a free variable no longer aliases an intervening caller's lexical

```raku
my $n = 1; my $a;
sub gn() { $a := $n }
sub fn() { my $n = 5; gn(); $n = 7; say $n }
fn();      # 7 in both
say $n;    # raku: 1     mutsu (before): 7
```

`fn`'s `my $n` and the compunit's `$n` are two independent variables, but after
`gn()` mutsu had fused them into one container: `fn`'s later write reached the
compunit lexical, and in the read-only spelling (`sub f() { my $v = 5; g(); $v }`)
`f` read the compunit's `1` instead of its own `5`.

## What was actually wrong

The ticket this closes had already ruled out the obvious story: free-variable
*reads* and *writes* resolve lexically today, at every nesting depth, exactly as
raku (`t/free-var-bind-does-not-alias-caller-lexical.t` rows A/B/F/S/U). The
divergence was confined to a `:=` bind whose source is a free variable, and its
shape was an *aliasing*, not a stale read.

The symptom surfaced at `exec_get_local_op_inner`'s lazy-sync adopt
(`src/vm/vm_var_assign_local_get.rs`): the caller's own slot, holding a plain
`5`, was overwritten with a `ContainerRef` the caller found in its **own env
overlay** under the same name. That adopt is not gratuitous — it is how a `:=`
performed in a callee reaches the declaring frame's slot — and it was already
narrowed once (from `get`/`get_sym` to `overlay_get`/`overlay_get_sym`) to stop
it picking up an *ancestor* frame's container. The remaining question was how a
foreign cell got into the caller's own tier at all.

Two writers, both in the `:=` handlers, and both by name:

1. **The bare `env` insert.** Both bind handlers — `exec_set_local_op_inner`'s
   scalar-bind path (`src/vm/vm_var_assign_set_local.rs`) and the `SetGlobal`
   twin (`src/vm/vm_exec_dispatch.rs`) — did
   `self.env_mut().insert(resolved_source, container)`. That lands in the
   *callee's* overlay, and the call-return merge
   (`call_compiled_function_named_inner`) copies every overlay entry that is not
   a callee local into the caller's tier. So the cell arrived in the caller's own
   overlay one statement later.
2. **`propagate_bind_to_ancestor_frames`.** Its gate picks the innermost ancestor
   frame whose `saved_env` owns the name *in its own tier*. A caller that
   re-declared the name has exactly such an entry — the `Nil` marker
   `exec_set_local_op_inner`'s redeclaration guard leaves behind precisely to say
   "this is a FRESH binding, do not inherit the outer cell". The splice read that
   marker as "this frame declares the name" and wrote the outer cell straight
   into it, which is the opposite of what it means.

(This is why the earlier A/B that disabled the splice "changed no row": with the
`env` insert still in place, route 1 delivered the same cell on its own. Each
route had to be closed before the other became visible.)

## The fix

For a name whose home is the compunit / mainline file-scope lexical store
(ADR-0024's `unit_lexicals`), that store *is* the lexical answer — its bare `env`
key belongs to whatever scope is calling us, which is the collision the store
exists to end. A new `Interpreter::unit_scope_lexical_bind`
(`src/vm/vm_env_helpers.rs`), the bind companion of the existing
`unit_scope_lexical_write`, rebinds the store's cell in place and reports that it
handled the name. Both bind handlers now skip *both* by-name routes when it does.

The binding itself is untouched: a later write to the source still reaches the
alias, in both the `SetGlobal` and the `SetLocal` spelling (rows H2 / Q2).

## Result

`t/free-var-bind-does-not-alias-caller-lexical.t` grew from 19 rows with four
`todo` to 22 rows with none — all 22 green under mutsu and under raku v2026.07.
The four that flipped are G2, H, N2 and O; H2/Q1/Q2 are new, pinning the half the
fix had to preserve.

## What remains

A lexical declared in a **bare block** has no equivalent store, so both by-name
routes still reach a shadowing caller there
(`todo/deep/free-var-lexical-resolution-inside-a-bare-block.md`). Closing that is
the env-model change ADR-0055 §7.5 names — a routine's env parent should be its
lexical scope, not its caller — and still wants its own ADR.
