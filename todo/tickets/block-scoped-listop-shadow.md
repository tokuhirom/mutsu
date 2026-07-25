# A block-scoped `push`/`pop` shadow does not suppress the listop rewrite

Found 2026-07-25 while closing `TODO_dist` T-054 (`P5push`) — see
`news/2026-07/listop-rewrite-respects-user-routine-shadow.md`. Pre-existing
behaviour, not a regression from that fix.

## Repro

```raku
{
    my sub push(@a, *@v) { "mine" }
    my @x = 1;
    say push(@x, 2);   # raku: mine     mutsu: [1 2]  (and @x was mutated)
}
```

The same applies to a block-scoped import:

```raku
{
    use ListopShadow;   # exports a Perl 5 style `push`
    my @a = 1;
    say push(@a, 42);   # raku: 2       mutsu: [1 42]
}
```

At unit scope both forms work (that is what T-054 needed and what
`t/listop-shadow-imported.t` / `t/listop-shadow-declared.t` pin).

## Why

`compiler/expr_call.rs` rewrites the container listops into method calls at
compile time (`pop(@a)` → `@a.pop()`, …) so array mutation reaches the caller's
container. `listop_shadowed_by_user_routine` suppresses that rewrite when a user
routine of the same name is visible, using the parser's thread-local lexical
scope stack (`parser::is_imported_function` /
`parser::is_user_declared_sub_pub`).

That stack is a *parse-time* structure. The compiler runs after the parse has
finished, so every scope that was pushed and popped during parsing is gone; only
scopes still live at the end — in practice the unit scope — can be queried. A
`my sub push` (or a `use`) inside a block was registered into a scope that no
longer exists, so the predicate returns false and the rewrite fires.

## Why it is not a small fix

The right fix is to stop asking the parser and instead decide at the point where
lexical scoping is actually modelled:

- **Option A — record it in the AST at parse time.** While the name *is* in
  scope, the parser knows whether a user routine shadows the builtin; it could
  mark the call node (e.g. a flag on `Expr::Call` / `Stmt::Call`, or emit a
  distinct node). This is the semantically correct place, but it widens the AST
  and every construction/match site for those variants.
- **Option B — track shadowed listop names in the compiler's own scope stack.**
  The compiler already pushes/pops scopes for locals, so it could carry a
  `shadowed_listops: Vec<HashSet<String>>` alongside, populated from `SubDecl` /
  `ProtoDecl`. The complication is `hoist_sub_decls`: a sub declaration is
  visible before its textual position, so the set has to be seeded when a block
  is entered, not when the declaration is reached.

A conservative unit-wide AST scan ("any `sub push` anywhere in the file → never
rewrite") is *not* an acceptable shortcut: it would disable the rewrite in scopes
where the builtin is the correct target, and the generic call path does not
guarantee the in-place array mutation the rewrite exists to provide.

## Affected files

- `src/compiler/expr_call.rs` — `listop_shadowed_by_user_routine` and the four
  rewrite branches it guards
- `src/parser/mod.rs` — `is_imported_function`, `is_user_declared_sub_pub`
- `src/parser/stmt/simple/{compile_consts,pragma_preseed}.rs` — the scope stack

## Impact

Low in practice: real distributions export these names at unit scope (P5push
does), which now works. A block-scoped shadow silently calls the builtin — wrong
value, and it mutates the array the user routine meant to leave alone.
