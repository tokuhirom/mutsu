# An adjacent colonpair truncated a no-paren call's argument list

`foo :!d:r, "x", "y"` — a listop-style call whose argument list starts with two
or more colonpairs written back-to-back — silently parsed as a two-element list
expression, `(foo(:!d, :r), "x", "y")`, instead of the single call
`foo(:!d, :r, "x", "y")`. There was no parse error: the script ran to completion
and produced the wrong result. The bug was found while writing a regression test
for `chdir`'s adverb handling (`news/2026-08/chdir-adverbs-parsed-as-the-path.md`)
but had nothing to do with `chdir` in particular.

## Root cause

The ticket's hypothesis — "a precedence rule finishes the argument list early
after a colonpair chain of length >= 2" — was close in effect but wrong in
mechanism, and it also described the trigger too narrowly. The trigger is not
*chaining*: `foo :a :b, "x"` (space-separated, still no comma) misparsed
identically. What matters is that a colonpair follows another argument with no
comma between them.

Raku's no-paren argument lists accept exactly that: `f :a :b, $x` and
`f :a:b, $x` both mean `f(:a, :b, $x)`, and the argument list continues past the
adjacent colonpairs. mutsu has four separate loops that parse such a list, and
only one of them knew this:

| argument-list loop | serves | had the rule |
| --- | --- | --- |
| `make_call_expr_from_listop_args` (`primary/ident/listop.rs`) | declared/imported/hyphenated subs | yes |
| `parse_remaining_call_args` (`stmt/args.rs`) | statement-level calls | yes |
| `parse_expr_listop_args` (`primary/ident/listop.rs`) | expression listops (`ok`, `is`, `diag`, ...) | **no** |
| the builtin-listop loop (`primary/ident/identifier_call.rs`) | `sort`, `sprintf`, `chdir`, ... | **no** |
| the generic bareword fallback (same file) | not-yet-declared / forward-referenced names | **no** |

In the three loops without the rule, only the *first* colonpair became an
argument. The loop then saw a `:` where it wanted a `,`, decided the argument
list was over, and returned the call with `:b, "x", "y"` still unconsumed.

What made the failure silent — and what disguised it as a chaining problem — is
what happened next. The postfix loop's *call-adverb* rule
(`expr/postfix/loop_.rs`, `supports_postfix_call_adverbs`) picked up the
leftover `:b` and appended it to the finished call's argument list. So the call
ended up with both named arguments and looked correct in an AST dump; but that
rule can only append to an already-built call, it cannot resume the listop's
argument list. The comma after `:b` was therefore left for the enclosing
list-expression parser, which happily read `foo(:a, :b), "x", "y"` as a
three-element list. With a single colonpair there is nothing left over, the
listop's own loop reaches the comma itself, and the call parses correctly —
which is why chain length appeared to be the discriminator.

## Fix

`try_adjacent_colonpair_arg` in `src/parser/primary/ident/listop.rs` is now the
one place that expresses the rule, and every no-paren argument-list loop
consults it before concluding that a missing comma ends the list.
`make_call_expr_from_listop_args` was rewritten to use it, and the three loops
that lacked it (`parse_expr_listop_args` and both loops in
`primary/ident/identifier_call.rs`) now call it too. The leftover colonpair
never reaches the postfix call-adverb rule for these shapes, so the argument
list continues normally through the following comma.

All three previously-broken flavours now agree with rakudo:
`foo :a:b, "x"` for a forward-referenced sub, `chdir :!d:r, $path` for a builtin
listop, and `ok :a:b, 'desc'` for an expression listop.

## Tests

`t/listop-adjacent-colonpair-args.t` pins the shape across all of them: a single
adverb, comma-separated adverbs, two and three chained adverbs, space-separated
adjacent adverbs, negated (`:!d`) and valued (`:a<1>`, `:a(1)`, `:a:$v`) adverbs,
adverbs after and between positionals, and — for the builtin-listop path — an
element-count assertion that fails outright if the call degenerates into a list
again. The whole file also passes under `raku`.
