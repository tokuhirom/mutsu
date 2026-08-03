# A `for` statement modifier is not a placeholder scope

`Digest::MD5` and `Digest::RIPEMD` both build their message block with

    $^b.push($_) for (@$msg, 0x80, 0x00 xx …).flat.rotor(4).map({ :256[@^a.reverse] });

and mutsu read `$^b` as `True`. Reduced:

    my $f = { say $^b for (1, 2) }; $f(42);
    # raku:  42 42
    # mutsu: True True

## The AST could not tell the two `for` forms apart

`collect_ph_stmt_shallow`'s `Stmt::For` arm deliberately did NOT descend into
the loop body, and that is right for a `for` **block**: its body is its own
placeholder scope, so `for @a { $^x }` gives the *loop* the parameter and the
enclosing block must not also claim it. But a `for` **statement modifier** is not
a block — its body is one statement evaluated in the enclosing scope — and both
forms were spelled as `Stmt::For { param: None, params: [] }`, indistinguishable.

`Stmt::For` now carries `is_statement_modifier: bool`, set only by the modifier
production in `parser/stmt/modifier.rs`, matching the `is_statement_modifier`
`Stmt::Given` already had. The three placeholder walkers in `ast.rs`
(`collect_ph_stmt_shallow`, `collect_unattached_ph_stmt`, `check_bare_var_stmt`)
descend into the body only for the modifier form. Codegen ignores the flag: the
two forms compile identically.

## The plain spelling of a placeholder

Collecting `$^b` correctly then exposed the next line of the same block:

    $^b.push($_) for …;
    $b.write-uint64: $b.elems, $bits, LittleEndian;

`$^b` declares the parameter under its *plain* name, so the later `$b` is the
same variable. mutsu enforces the ordering rule ("a bare `$b` written *before*
its `$^b` is `X::Undeclared`") with `bare_precedes_placeholder`, whose walker
did not look inside a `Stmt::VarDecl`'s initializer or a `for` modifier's body —
so `{ my $z = $^b; say $b }` and `{ say $^b for 1; say $b }` both reported the
later `$b` as undeclared. Both arms were added, checked against rakudo:

| block                             | rakudo     | reason                              |
| --------------------------------- | ---------- | ----------------------------------- |
| `{ my $z = $^b; say $b }`         | ok         | same statement scope                |
| `{ say $^b for 1; say $b }`       | ok         | a modifier body is this scope       |
| `{ for 1 { $^b }; say $b }`       | undeclared | the loop block owns the placeholder |
| `{ if 1 { $^b }; say $b }`        | undeclared | the `if` block owns it              |
| `{ say $b; say $^b }`             | undeclared | bare use precedes the placeholder   |

Rows 1, 2 and 5 are pinned in `t/for-modifier-placeholder-scope.t`, which passes
under rakudo as well as mutsu. Rows 3 and 4 stay **known false negatives**:
mutsu accepts both. That gap predates this change and is a separate defect in
`bare_precedes_placeholder` — the scan is per top-level statement and never
descends into a nested block's body, so it cannot see that the inner `$^b`
belongs to the inner block rather than to this one. Fixing it means giving the
walker the same scope discipline the placeholder collectors have; it is not
needed by the `Digest` distribution, so it is left recorded here rather than
attempted alongside.

`Digest::MD5` now runs to completion; its digest is still wrong, which is a
further bug tracked in `todo/tickets/digest-dist-blockers.md`.
