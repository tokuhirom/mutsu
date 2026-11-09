# A slang can re-write the core `for` token (Slang::Otherwise)

Slang::Otherwise declares its own `statement-control:sym<for>` token
(`<.block-for><.kok> <EXPR> <pointy-block> [ otherwise <pointy-block> ]?`) and an
action that `callsame`s into the core one and calls `replace-otherwise`. The
slang host now has what that token calls: `block-for`, `vetPerlForSyntax` and
the `<EXPR>` / `<pointy-block>` shims (the core parser reads the `for` header
and block), and the core `for` action builds the `RakuAST::Statement::For` from
those captures.

Along the way: a silent call that carries a node (`$<x>=<.pointy-block>`) keeps
it for the alias; registering a production invalidates the parse memo, so the
statement right after the activating `use` is parsed under it; and a
`RakuAST::Statement::For` with an `otherwise` block now runs it when the loop
produced nothing (`(for ...) || do { otherwise }`), where it used to be stored
and ignored. Slang::Otherwise's own `t/01-basic.rakutest` passes (#13370).
A plain `for` without `otherwise` under that slang also works here; rakudo dies
on it (`Cannot find method 'set-statement-id'`) because the action attaches
nothing, so the test does not pin it.
