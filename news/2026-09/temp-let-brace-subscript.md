# `temp %h{$key} = ...` and `let %h{$key}` parse

`Pod::To::PDF::Lite`'s writer guards against recursive replacement with

```raku
temp  %!replacing{$place-holder} = True;
```

and mutsu could not compile the module: the error surfaced as this #7988
cluster's generic `Confused. expected statement: expected expression statement
or ')'`, reported at the enclosing `method !replace(...)` header rather than
at the `temp`.

`let_stmt` / `temp_stmt` (`src/parser/stmt/simple_expr_stmt/let_temp.rs`)
handled a subscripted element only as `@a[i]` or `%h<k>`; the brace form
`%h{EXPR}` fell through and failed. The two parsers carried identical copies of
the `[...]` and `<...>` branches, so they are now one `let_subscript_stmt`
helper that also accepts `{EXPR}` (the same `Stmt::Let` with an index, which
the runtime already restores for a hash). The file shrinks from 560 to under
500 lines as a result.

Pinned by `t/collections/subscript/temp-let-brace-subscript.t`, including a
`temp` on an attribute hash element. `Pod::To::PDF::Lite::Writer` now parses;
loading it next stops in its PDF dependency (`PDF::COS::Tie`'s
`COSDictAttrHOW` export tag), a separate gap.
