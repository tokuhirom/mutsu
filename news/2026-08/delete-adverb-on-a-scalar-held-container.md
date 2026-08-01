# `:delete` through a `$`-held container no longer destroys the variable

`%h<k>:delete` worked. The same subscript on a container held in a `$` deleted
nothing, returned the wrong thing, and left the variable holding `Any`:

```raku
my $h = { a => 1, b => 2 };
say ($h<a>:delete).raku;   # raku: 1        was: Nil
say $h.raku;               # raku: ${:b(2)} was: Any    <-- the hash was gone
```

Losing the variable was the serious half: a program that deleted a key from a
hash it happened to hold in a `$` silently continued with an undefined value.
It was not hash-specific — every `$`-held container went the same way
(`my $a = [1,2,3]; $a[1]:delete` and `my $s = <a b>.SetHash; $s<a>:delete` both
answered `Any`).

## The dual store, and where the `Any` came from

The AST is identical to the `%`-sigil case apart from the variable node: both
lower to `MethodCall { target: Index { .. }, name: "DELETE-KEY" }`, both reach
`Compiler::compile_expr_method_generic`'s `:delete` lowering, and both emit
`OpCode::DeleteIndexNamed`. The two only differ in where the container lives.

`exec_delete_index_named_op` (`src/vm/vm_var_delete_ops.rs`) resolved its
container out of `env` by name, and a scalar-held container is not there: it
lives in the local slot, and the env mirror stays at the `my`-declaration seed —
a bare `Any` type object. So the op did find "a container", deleted nothing from
it (no `Hash`/`Array`/quanthash arm matches a type object, so the fall-through
returned `Nil`), and then its closing env-to-slot sync wrote that `Any` straight
over the variable. That last step is what destroyed the hash; the earlier
diagnosis of "the lookup misses and the writeback stores the hole it produced"
had the shape right but the wrong value — the env entry existed, it was just the
declaration seed.

The fix is the one the *element-assignment* handler already had. That handler
(`exec_index_assign_expr_named_op_inner`, the "(B) per-store env-write" step)
hit exactly this problem for `my $b = "hi".encode; $b[0] = 200`, and solved it by
refreshing the env mirror from the authoritative slot before the env-centric body
runs. That block is now the shared helper `Interpreter::seed_env_from_scalar_slot`,
and `:delete` calls it too. It keeps the original restrictions, which are
load-bearing: only a bare scalar name seeds (an `@`/`%` aggregate keeps its
container in env, where the representation may be more reified than the slot's —
seeding would clobber a lazy prefix), and only when the slot holds a real value
whose variant differs from the mirror.

## What this unblocks

Reading the live container also makes the correctness checks on that path
reachable for the `$`-held spelling, which had been silently skipped:

```raku
my $m = Map.new("a", 1); $m<a>:delete;   # now X::AdHoc, as `my %h is Map` already did
my $s = Set.new(<a b>);  $s<a>:delete;   # now X::Assignment::RO
```

Pins: `t/delete-adverb-on-scalar-held-container.t` (hash, `{}` and `<>`
spellings, slices, absent keys, nested deletes, arrays with interior and
trailing holes, SetHash/BagHash, and the immutable Set/Bag refusals), plus three
`$`-held Map assertions added to `t/map-delete-is-refused.t`. Every assertion in
both files passes unmodified under rakudo.

One divergence is deliberate and unchanged: for a slice delete from a Map, rakudo
answers a lazy list of `Failure`s while mutsu throws at the subscript. The tests
assert what both runtimes agree on — that nothing is removed.
