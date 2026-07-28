# A ternary's then-branch cannot be an enum value

`COND ?? SOME_ENUM_VALUE !! OTHER` fails to parse whenever the parser reaches it
in `ExprMode::Full`. Minimal repro:

```raku
enum E <FOO BAR>;
my $o;
given $o { .x = 1 ?? FOO !! 2 }
# Parse error: Unexpected block in infix position
#   (missing statement control word before the expression?)
```

raku accepts it. The same shape with a literal (`1 ?? 2 !! 3`), a type object
(`1 ?? Int !! Str`), a `constant` (`constant FOO = 1`) or a sigilless term
(`my \foo`) all parse — only enum values fail.

## Root cause

`src/parser/expr/precedence/ternary.rs:222-245` rejects a `BareWord` in
then-position unless it names something the parser knows is a complete nullary
term. The guard exists for a real reason, pinned by
`roast/S03-operators/ternary.t:121` and `t/ternary-type-branch.t:49`: raku
throws `X::Syntax::ConditionalOperator::SecondPartGobbled` for
`1 ?? rt123115 !! 3` when `rt123115` is a declared routine, because the listop
swallows the `!!`.

Its allow-list is `is_known_type_constraint` / `is_builtin_enum_value` /
`is_builtin_constant_term` / `is_user_declared_type` /
`is_user_declared_value_term`. **User-declared enum *values* are in none of
them.** `collect_module_type_names_under`
(`src/parser/stmt/simple/module_exports.rs:392`) registers a `Stmt::EnumDecl`'s
*type* name from a `use`d module but not the value names it exports, and a
locally declared `enum` does not register its values as term symbols either.

An enum value is by construction a complete nullary term — it can never be a
listop head — so it belongs in the allow-list.

## Why it matters

It is what now blocks `DBIish`'s mysql driver, the last of the nine files
(`todo/tickets/dbiish-blockers.md` ⑨). `DBDish::mysql::StatementHandle` has

```raku
.buffer_type = @!column-type[$col] ~~ Blob
        ?? MYSQL_TYPE_BLOB !! MYSQL_TYPE_STRING;
```

where both names come from `enum mysql-field-type is export (…)` in
`DBDish::mysql::Native`, so the whole module fails to parse and
`DBIish.install-driver('mysql')` dies. ADR-0015 P2 (the REPR body) is landed and
is no longer the blocker.

Note that the failure is **mode-dependent**, which is why it looks arbitrary:
`my $y = 1 ?? FOO !! BAR` parses (the declaration RHS is read in a non-`Full`
mode, where the guard is skipped) while `$o.x = 1 ?? FOO !! BAR` does not (the
method-lvalue statement path goes through `parse_assign_expr_or_comma` →
`parse_comma_or_expr`, i.e. `Full`).

## Shape of the fix

Register user-declared enum value names as `TermBinding::Value` term symbols, in
both places:

- a local `enum` declaration (wherever `Stmt::EnumDecl` is parsed);
- an enum harvested from a `use`d module — `collect_module_type_names_under`
  currently pushes only the enum's own name, so the value names need their own
  channel (they are terms, not types, so `register_user_type_verbatim` is the
  wrong sink; `register_user_term_symbol` is the right one).

`is_user_declared_value_term` then answers for them and the guard stops firing,
with no change to the guard itself — so the `rt123115` roast case keeps failing
exactly as it should.

Worth checking while there: whether the guard should also be skipped whenever
the very next token is `!!` (a listop that had gobbled the `!!` would not have
left one to see). That would be a broader relaxation than this ticket needs, and
it interacts with the roast case, so measure before taking it.
