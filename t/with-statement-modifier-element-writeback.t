use Test;
plan 5;

# `t/with-element-topic.t` pins the *block* form (`with EXPR { ... }`), which
# already wrote a container-element topic back correctly. The statement-
# *modifier* form (`STMT with EXPR`) took a different compile path when the
# statement was itself an expression statement (e.g. `.=Int with @a[i]`): the
# parser wraps it in `Expr::DoStmt(Stmt::Given)` to preserve expression
# semantics (`modifier.rs`), and the expression-form Given compiler
# (`compile_expr_do_stmt` in `expr_block.rs`) never had the element-source
# detection the statement-position compiler (`stmt.rs`) has — so the topic
# was pushed as a plain value copy and any mutation of `$_` (`.=Int`, `$_ = `)
# never reached the array/hash element.
#
# This is not a synthetic corner case: `Cro::HTTP::Router`'s route matcher
# does exactly `.=Int with @segs[2]` to convert a path segment to `Int` before
# building the route's argument Capture, so the vendored Cro suite's
# `http-router.rakutest` hung forever on any route with a typed optional
# trailing parameter (`Int $page?`) whose value was actually present — the
# unconverted `Str` failed to bind and the response was silently never
# produced.

{
    my @a = "1", "2";
    .=Int with @a[1];
    is-deeply @a, ["1", 2], '.=Int with @a[i] (statement modifier) writes the Int back to the element';
    is @a[1].WHAT.raku, 'Int', 'the element is now a real Int, not a Str';
}

{
    my %h = a => "1";
    .=Int with %h<a>;
    is %h<a>, 1, '.=Int with %h<k> (statement modifier) writes the Int back to the element';
}

{
    # A plain scalar variable topic already worked (`Expr::Var` gets
    # `TagContainerRef` in both compile paths) — pin it alongside the element
    # cases so a regression is caught in the same file.
    my $x = "1";
    .=Int with $x;
    is $x, 1, '.=Int with $x (statement modifier, plain scalar) still writes back';
}

{
    # `do given @a[i] { ... }` is the same DoStmt(Given) compile path with an
    # explicit block instead of a bare expression statement body.
    my @a = "1", "2";
    do given @a[1] { .=Int };
    is-deeply @a, ["1", 2], 'do given @a[i] { .=Int } writes the Int back to the element';
}
