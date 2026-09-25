# A hash subscript's `}` no longer ends the statement at a newline

`%h{"a"}` followed by a newline and `.method` used to misparse (#9330).
Outside parentheses it ran and gave a wrong answer: `my $v = %r{"a"}\n.Str`
assigned `%r{"a"}` and then called `.Str` on `$_` as a separate statement,
leaving `$v` as the stringified hash value only by luck and warning about an
uninitialised `Any`. Inside parentheses it was a `Confused` parse error.

The cause was Raku's line-ending-block rule ("a `}` that is the last thing on
its line terminates the statement"). mutsu applied it textually, to any
consumed span ending in `}`, at three sites: the postfix loop's whitespace-dot
check, `prefix_expr_with_ws_dot`, and `block_newline_terminates` (the infix
side). A `}` that closes a postcircumfix subscript is not a block boundary, so
rakudo keeps chaining across the newline. The three sites now consult a shared
`is_subscript_expr` helper on the parsed AST (`Index`, `MultiDimIndex`,
`ZenSlice`) and skip the rule for a subscript. `%r<a>` and `@r[0]` were never
affected, since they do not end in `}`.

It was found while reducing fez 100.0.2 (`lib/Fez/CLI.rakumod`, a
`%rsult{$k}` newline `.keys` newline `.sort` chain). The regression test is
`t/collections/subscript/hash-subscript-brace-newline-continues.t`. It covers the
assignment and parenthesised forms, nested, slice, multi-dim and zen
subscripts, an infix continuation, and a real block-final `}` that must still
end its statement.
