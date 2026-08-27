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
| `parse_expr_listop_args` (`primary/ident/listop.rs`) | expression listops (`indir`, `flat`, `first`, ...) | **no** |
| the builtin-listop loop (`primary/ident/identifier_call.rs`) | `sort`, `sprintf`, `chdir`, ... | **no** |
| the generic bareword fallback (same file) | not-yet-declared / forward-referenced names | **no** |
| `parse_expr_list` (`stmt/simple/io_stmts.rs`) | `say`, `print`, `put`, `note` | **no** |

In the four loops without the rule, only the *first* colonpair became an
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

The io listops fail one step earlier, and more loudly. `say`/`print`/`put`/
`note` are parsed by their own statement handler
(`src/parser/stmt/simple/io_stmts.rs`), which tries the *invocant colon* form
(`say $*OUT: "hi"`) before the ordinary argument list. With no guard against a
colonpair, `say :!d:r, "x"` parsed the leading `:!d` as the invocant and the
second colon as the invocant marker, yielding `(:!d).say(r, "x")` — which dies
with `No such method 'say' for invocant of type 'Pair'`.

## Fix

`try_adjacent_colonpair_arg` in `src/parser/primary/ident/listop.rs` is now the
one place that expresses the rule, and every no-paren argument-list loop
consults it before concluding that a missing comma ends the list.
`make_call_expr_from_listop_args` was rewritten to use it, and the four loops
that lacked it (`parse_expr_listop_args`, both loops in
`primary/ident/identifier_call.rs`, and `parse_expr_list` in `io_stmts.rs`) now
call it too. The leftover colonpair never reaches the postfix call-adverb rule
for these shapes, so the argument list continues normally through the following
comma.

The invocant-colon guards were factored out alongside it as
`colon_starts_colonpair` (a colon that binds a name or sigil with no space opens
a colonpair, not an invocant marker) and `expr_is_colonpair` (a colon that
*follows* a colonpair likewise opens another one). `try_parse_no_paren_invocant_colon_call`
had both inline; `parse_io_colon_invocant_stmt` had neither, and now shares
them. `say $*OUT: "hi"` is unaffected — its colon is followed by whitespace.

All the previously-broken flavours now parse as rakudo does:
`foo :a:b, "x"` (forward-referenced sub), `chdir :!d:r, $path` (builtin listop),
`indir :!d:r, $path, { … }` (expression listop), and `say :!d:r, "x"`
(io listop).

One divergence deliberately remains out of scope and is filed as
`todo/tickets/io-listops-bind-colonpair-args-as-positional.md`: the io listops
bind a colonpair as a *positional* argument and print it, where raku binds it as
a named argument that never reaches the output. That is argument-binding
semantics (ADR-0021 pair-namedness territory), not parsing, and it predates this
fix — it is equally visible in the single-adverb `say :d, "x"`.

## Tests

`t/listop-adjacent-colonpair-args.t` pins the shape across all of them: a single
adverb, comma-separated adverbs, two and three chained adverbs, space-separated
adjacent adverbs, negated (`:!d`) and valued (`:a<1>`, `:a(1)`, `:a:$v`) adverbs,
and adverbs after and between positionals. Because each call flavour has its own
argument-list loop, the file also exercises each one separately: a
forward-referenced (non-hyphenated) bareword for the generic fallback, `chdir`
for the builtin listop — with an element-count assertion that fails outright if
the call degenerates into a list again — `indir` for the expression listop, and
`note` (which writes to stderr, so it does not disturb the file's own TAP
stream) for the io listops. The whole file also passes under `raku`.
