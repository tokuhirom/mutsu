# A multi-parameter `for` loop no longer mutates an `@`/`%` container it shadows

```raku
my @arr = (100, 200);
for 1, [10,20], 2, [30,40] -> $a, @arr { }
say @arr;   # raku: [100 200]   mutsu (before): [30 40]
```

`build_for_bind_stmts` bound every multi-param loop variable — regardless of
sigil — via a plain `Stmt::Assign`, reusing whatever local slot the name
already occupied instead of giving it a fresh one. For a scalar name that slot
holds a plain value, so a snapshot-and-restore around the loop (the sibling
fix in `for-multi-param-shadow-clobbers-outer-lexical.md`) was enough. For an
`@`/`%`-sigil name the slot holds a container whose *contents* get mutated in
place — exactly what lets a real `@arr = (...)` re-assignment preserve
identity for other aliases — so when the loop parameter reused the outer
`@arr`'s own slot, each iteration's bind mutated the SAME container the outer
variable aliases. A snapshot-and-restore of the slot's `Value` only clones the
handle, not the contents, so it restored a handle to a container that was
already overwritten.

Fixed at the root: an `@`/`%`-sigil multi-param loop variable is its own fresh
per-iteration lexical in Raku, not an alias of a same-named outer `@`/`%`.
`build_for_bind_stmts` now declares it (`Stmt::VarDecl`, the same shadowing
declaration a destructured sub-signature target already used) instead of
assigning into the shared slot, so each iteration gets a genuinely fresh
container and the outer variable is shadowed rather than clobbered. The
declaration is wrapped with the same `MarkBind` marker `my @a := expr` uses,
so the bind is raw (no `my @a = expr`-style coercion) — otherwise an
already element-typed source array (`array[int8]`) would collapse to a plain
untyped `Array`.

While reproducing the ticket, its noted scalar residual — a shadowed name
with no local slot in the frame, falling through to a global by-name write —
turned out to already be fixed by an earlier general change; a regression
case for it is added alongside the new `@`/`%` cases.

Pin: `t/for-multi-param-type-constraint.t`, extended with the global-write
scalar case and three `@`/`%`-sigil cases (outer array/hash survives
shadowing, and the loop parameter itself still binds each iteration's own
value).
