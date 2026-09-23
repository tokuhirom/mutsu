# `my $__x` declares `$__x`, not `$_`

A lexical whose name starts with a double underscore did not read back its
value: `my $__t0 = 5; say $__t0 + 1` printed `1` instead of `6`, and a timing
harness's `now - $__t0` printed the raw epoch.

The issue suspected a collision with an internal `__` naming convention, but
the cause was one level earlier, in the declarator's variable-name parser
(`var_name` in `src/parser/stmt/idents.rs`). Its `$_` special case accepted a
`_` followed by anything that was not an ASCII *alphanumeric* — and `_` is
not alphanumeric — so `my $__x = 5` was parsed as `my $_` plus an assignment
to an undeclared `_x`. Reading `$__x` elsewhere went through the ordinary
expression parser, which saw the whole name and found nothing bound to it.

The topic is now recognised only when the `_` is not followed by another
identifier character (a Unicode letter/digit or `_`), so `$__x` and `$_ä` are
ordinary names and `$_` is still the topic. `@__x`, `%__x` and `&__x` were
never affected (the special case is `$`-only).

Pinned by `t/vm/scope/double-underscore-lexical-names.t` (#9167).
