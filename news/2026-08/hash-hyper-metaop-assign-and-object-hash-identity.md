# Hash hyper-meta-assignment now writes back, and hyper ops keep object-hash identity

Two independent, pre-existing gaps in hyper operators on `%`-sigil hashes,
found while fixing a `for`-loop multi-param binding regression:

## `%x >>[&op]=<< %y` never wrote its result back to `%x`

```raku
sub op($x, $y) { ($x + $y).Str }
my %x = a => 1, b => 2;
my %y = a => 3, b => 4;
%x >>[&op]=<< %y;
say %x.raku;   # raku: (a => "4", b => "6")   mutsu (before): (a => 1, b => 2), unchanged
```

`compile_expr_hyper_op`'s assignment-form lvalue dispatch (`>>op=<<`,
`<<op=>>`, …) matched `Expr::ArrayVar` and `Expr::Var` to emit the
store-back, but had no `Expr::HashVar` case — a `%`-sigil left operand fell
through to "leave the result on the stack", so the whole assignment silently
became a no-op. The symbolic form (`>>+=<<`) was unaffected — it lowers to
`Expr::Var`-shaped compilation for a different reason and already worked.
Added the missing `Expr::HashVar` case, mirroring `Expr::ArrayVar`.

## A hyper op on two object hashes (`%h{Any}`) lost the object-hash identity

```raku
my %a{Any} = "a" => 1, "b" => 2;
my %b{Any} = "a" => 3, "b" => 4;
say (%a >>+<< %b).raku;
# raku:  $(my Any %{Any} = :a(4), :b(6))
# mutsu (before): ${"Str|a" => 4, "Str|b" => 6}
```

Both hyper-op hash-combining implementations (`hyper_op_pair` for symbolic
operators, `exec_hyper_func_op_hash` for the `>>[&func]<<` bracketed-function
form) rebuilt the result as a plain `Value::hash_with_data(Value::hash_arc(map))`
from the raw `.WHICH`-keyed storage map, with no per-key `original_keys`
lookup and no `key_type`/`value_type`/`declared_type` metadata — so the
result rendered its internal `.WHICH` strings as literal keys instead of the
real key objects, and lost the `{Any}` object-hash type identity entirely.
Both now merge `key_type`/`value_type`/`declared_type` (left operand
preferred, matching the existing key-set precedence) and the per-key
`original_keys` entries from whichever operand(s) carry them, and stamp the
merged metadata onto the rebuilt `HashData`.

Pin: `t/for-multi-param-writethrough-metadata.t`.
