# `:delete` through a `$`-held container is a no-op that also destroys the variable

`%h<k>:delete` works. The same subscript on a hash held in a `$` deletes
nothing, returns the wrong thing, and leaves the variable holding `Any`:

```raku
my $h = { a => 1, b => 2 };
say ($h<a>:delete).raku;   # raku: 1        mutsu: Any
say $h.raku;               # raku: ${:b(2)} mutsu: Any    <-- the hash is gone
```

Losing the variable is the serious half: a program that deletes a key from a
hash it happens to hold in a `$` silently continues with an undefined value.

The AST is *identical* to the `%`-sigil case apart from the variable node —
both lower to `MethodCall { target: Index { .. }, name: "DELETE-KEY", args: [] }`,
and both reach `Compiler::compile_expr_method_generic`'s `:delete` lowering,
which resolves `Self::postfix_index_name(delete_target)`
(`HashVar("h")` → `"%h"`, `Var("h")` → `"h"`) and emits
`OpCode::DeleteIndexNamed`. So the two differ only downstream of that opcode.

**It is the dual store.** `exec_delete_index_named_op_inner`
(`src/vm/vm_var_delete_ops.rs`) resolves its container with
`self.env_mut().get_mut(&var_name)`. A `%`-variable is in `env`; a mainline
`my $h` lives in a **local slot**, so the lookup misses, the op takes
`delete_from_missing_container`, and the writeback that follows stores the hole
it produced under the variable's name — which is what replaces the hash with
`Any`. The tell is that the same subscript works as soon as the variable is in
`env`:

```raku
my $h = { a => 1 }; $h<a>:delete;                # mainline: no-op, $h becomes Any
my $h = { a => 1 }; say (try { $h<a>:delete });  # inside a block: deletes correctly
```

So the fix is to resolve the container through the slot as well (the opcode
already carries `slot: Option<u32>` for exactly this) rather than by name only,
and to leave the variable alone when no container is found.

It is not hash-specific — every `$`-held container is destroyed the same way:

```raku
my $a = [1, 2, 3]; $a[1]:delete; say $a.raku;    # raku: $[1, Any, 3]   mutsu: Any
my $s = <a b>.SetHash; $s<a>:delete; say $s.raku; # raku: SetHash.new("b")  mutsu: Any
```

It also hides a correctness check that is otherwise in place: an immutable `Map`
refuses every removal ([news](../../news/2026-08/map-delete-is-refused.md)), and
that guard reads the container the same way — so a *mainline* `my $m =
Map.new("a", 1); $m<a>:delete` silently does nothing instead of dying, while the
identical subscript inside a block refuses correctly, as do `my %h is Map` and
`$m.DELETE-KEY("a")`. Fixing the container lookup makes the Map guard reachable
for the mainline `$`-held spelling too; the pin belongs in
`t/map-delete-is-refused.t` once it is.

Related but distinct: the slice spelling `$m<a b>:delete` answers `(Nil Nil)`.
