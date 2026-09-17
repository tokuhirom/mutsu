# File::Name::Editor: fix `.method (elem) $set` on the implicit topic invocant

`File::Name::Editor` was `blocked_load`: its own `get_cyclic_rename_list` sub
uses `if .value (elem) $rename_set { ... }` inside a `for` loop — a no-arg
method call on the topic (`$_`) immediately followed, with a space, by the
`(elem)` set-membership operator. mutsu rejected it with "Confused. no space
allowed between method name and the left parenthesis".

The explicit-invocant form of this construct (`@a.Set (|) @b.Set`) already had
an allowance for this in `src/parser/expr/postfix/loop_.rs`: before flagging a
space-then-`(` as the "no space before call parens" error, it checks whether
what follows is actually a `(...)`-delimited set/baggy infix operator rather
than a parenthesized argument list, and if so leaves it for the general infix
parser. The *implicit*-invocant form (`.value (elem) $set`, parsed in
`src/parser/primary/regex/lit.rs` as a topic method call) had the identical
"detect illegal space" check but was missing that allowance entirely, so it
always errored.

Rather than duplicate (and now double-duplicate) the same hardcoded operator
list, the check was factored into a single `starts_with_set_infix_op()` in
`precedence_meta_ops::set_ops` — built on the module's own canonical
`parse_set_op()`, so it also picks up the Unicode glyph spellings (`∈`, `∪`,
...) and the `(<+)`/`(>+)` legacy aliases that the old hardcoded list in
`loop_.rs` didn't cover. Both call sites now use it.

`File::Name::Editor` now loads cleanly under mutsu and moves from
`blocked_load` to `no_baseline` (the distribution ships no test files at all,
so there is nothing further to measure). Pinned by two new cases in
`t/oo/method/set-op-after-method.t` covering the topic form.
