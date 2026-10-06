use Test;

# Control flow and phasers in RakuAST, measured on rakudo 2026.09:
#
# - `STMT for LIST` is the statement with `loop-modifier =>
#   StatementModifier::For(LIST)`, not a `Statement::For` around a block; the
#   same inside `[...]` and `(...)`;
# - `hyper for` / `race for` / `lazy for` are a `Statement::For` with that
#   `mode`, wrapped in a statement; `for @a <-> $x { }` has `default-rw`
#   parameters and `for @a -> { }` a pointy block with no signature;
# - `CONTROL { ... }` is `Statement::Control` over the same exception block as
#   `CATCH`;
# - `if EXPR -> $v { }` (and `elsif`) has a `PointyBlock` for its `then`;
# - a phaser in expression position (`my $x = BEGIN { 1 }`) is the same
#   `StatementPrefix::Phaser::Begin` node, `once { }` a `StatementPrefix::Once`;
# - `last FOO` / `next FOO` call with a `Term::Name` argument;
# - `do for ...` / `do given ...` is `StatementPrefix::Do` over the statement.
#
# The round trip is the parsed program. The tree part of this file also passes
# under `raku`; the round trip part is mutsu's.

plan 57;

sub stmts($src) { $src.AST.statements }
sub same($src, $expected, $desc) {
    my $parsed = do { my @*LOG; EVAL($src) };
    my $round = do { my @*LOG; EVAL($src.AST) };
    is "$parsed|$round", "$expected|$expected", $desc;
}

# --- the statement modifier `for`
{
    my $s = stmts(Q[say $_ for 1, 2])[0];
    isa-ok $s, RakuAST::Statement::Expression, '`STMT for LIST` is a Statement::Expression';
    isa-ok $s.loop-modifier, RakuAST::StatementModifier::For, 'with a For loop-modifier';
    isa-ok $s.loop-modifier.expression, RakuAST::ApplyListInfix, 'over the list';
    isa-ok $s.expression, RakuAST::Call::Name::WithoutParentheses, 'the modified statement stays an expression';
    my $c = stmts(Q[my @a = [5 for 1, 2]])[0].expression.initializer.expression;
    isa-ok $c, RakuAST::Circumfix::ArrayComposer, 'an array composer';
    my $inner = $c.semilist.statements[0];
    isa-ok $inner.loop-modifier, RakuAST::StatementModifier::For, 'holds the modified statement';
    my $p = stmts(Q[my @a = ($_ * 2 for 1..3)])[0].expression.initializer.expression;
    isa-ok $p.semilist.statements[0].loop-modifier, RakuAST::StatementModifier::For, 'so does a parenthesis';
    my $b = stmts(Q[{ say $_ } for 1, 2])[0];
    isa-ok $b.expression, RakuAST::Block, 'a block is the modified expression';
}

# --- for loops
{
    my $h = stmts(Q[hyper for 1, 2 { $_ }])[0];
    isa-ok $h, RakuAST::Statement::Expression, '`hyper for` is a statement around the loop';
    is $h.expression.mode, 'hyper', 'with its mode';
    is stmts(Q[race for 1, 2 { $_ }])[0].expression.mode, 'race', '`race for`';
    is stmts(Q[lazy for 1, 2 { $_ }])[0].expression.mode, 'lazy', '`lazy for`';
    is stmts(Q[for 1, 2 { $_ }])[0].mode, 'serial', 'a plain for is serial';
    my $rw = stmts(Q[my @a; for @a <-> $x { $x }])[1];
    isa-ok $rw.body, RakuAST::PointyBlock, '`<->` has a pointy block';
    ok $rw.body.signature.parameters[0].default-rw, 'whose parameter is `default-rw`';
    my $z = stmts(Q[for 1, 2 -> { 1 }])[0];
    isa-ok $z.body, RakuAST::PointyBlock, '`-> { }` is a pointy block';
    is $z.body.signature.parameters.elems, 0, 'with no parameters';
}

# --- CONTROL, `if -> $v`, phasers, labels, do
{
    my $c = stmts(Q[CONTROL { default { 1 } }])[0];
    isa-ok $c, RakuAST::Statement::Control, '`CONTROL { }` is a Statement::Control';
    isa-ok $c.body, RakuAST::Block, 'over a block';
    my $i = stmts(Q[if 1 -> $v { $v } elsif 2 -> $w { $w } else { 3 }])[0];
    isa-ok $i.then, RakuAST::PointyBlock, '`if EXPR -> $v` has a pointy then';
    is $i.then.signature.parameters[0].target.name, '$v', 'with its parameter';
    isa-ok $i.elsifs[0].then, RakuAST::PointyBlock, 'so does an `elsif`';
    my $x = stmts(Q[my $x = BEGIN { 1 }])[0].expression.initializer.expression;
    isa-ok $x, RakuAST::StatementPrefix::Phaser::Begin, 'a phaser in expression position';
    isa-ok stmts(Q[once { 1 }])[0].expression, RakuAST::StatementPrefix::Once, '`once { }`';
    my $l = stmts(Q[FOO: for 1 { last FOO }])[0].body.body.statement-list.statements[0].expression;
    is $l.name.canonicalize, 'last', 'a labelled last is a call';
    isa-ok $l.args.args[0], RakuAST::Term::Name, 'over the label as a name';
    my $d = stmts(Q[my @a = do for 1, 2 { $_ }])[0].expression.initializer.expression;
    isa-ok $d, RakuAST::StatementPrefix::Do, '`do for` is a StatementPrefix::Do';
    isa-ok $d.blorst, RakuAST::Statement::For, 'over the loop';
    my $g = stmts(Q[my $x = do given 5 { $_ }])[0].expression.initializer.expression;
    isa-ok $g.blorst, RakuAST::Statement::Given, '`do given`';
}

# --- the round trip is the parsed program
same Q[my @a; push @a, $_ * 2 for 1..3; @a.join(",")], '2,4,6', 'a for modifier';
same Q[my @r = [$_ * 2 for 1..3]; @r.join(",")], '2,4,6', 'in an array composer';
same Q[ [5 if 1].elems ~ "," ~ [5 if 0].elems], '1,0', 'an if modifier in a composer';
same Q[my @r = ($_ * 2 for 1..3); @r.join(",")], '2,4,6', 'in a parenthesis';
same Q[my $s = 0; { $s += $^a * $^b } for (1, 2), (3, 4); $s], 4, 'a placeholder block keeps its arity';
same Q[my $s = 0; { $s += $_ } for 1, 2, 3; $s], 6, 'a plain block';
same Q[my @a = 1, 2, 3; for @a <-> $x { $x *= 2 }; @a.join(",")], '2,4,6', 'a `<->` loop writes back';
same Q[(try { for 1, 2 -> { 1 }; "ok" }) // "dies"], 'dies', 'a zero-parameter block dies';
same Q[my @r = (hyper for 1, 2 { $_ * 2 }); @r.sort.join(",")], '2,4', 'a hyper loop';
same Q[{ CONTROL { when CX::Warn { @*LOG.push("warned"); .resume } }; warn "boo"; @*LOG.push("after") }(); @*LOG.join(",")], 'warned,after', 'a CONTROL block handles a warning';
same Q[if 1 -> $v { $v + 1 } else { 0 }], 2, '`if EXPR -> $v`';
same Q[if 0 -> $v { $v } elsif 5 -> $w { $w } else { 0 }], 5, 'and its `elsif`';
same Q[my $x = BEGIN { 7 }; $x], 7, 'a phaser expression';
same Q[my $c = 0; sub f1 { once { $c++ }; $c }; f1(); f1()], 1, '`once` runs once';
same Q[my $n = 0; FOO: for 1, 2 { for 3, 4 { $n++; next FOO } }; $n], 2, 'a labelled next';
same Q[my $n = 0; FOO: for 1, 2 { for 3, 4 { $n++; last FOO } }; $n], 1, 'a labelled last';
same Q[my @a = do for 1, 2 { $_ * 3 }; @a.join(",")], '3,6', '`do for`';
same Q[my $x = do given 5 { $_ + 1 }; $x], 6, '`do given`';
same Q[my $s = (given 5 { $_ * 2 }); $s], 10, 'a parenthesised given';
same Q[my $x = do if 1 { 4 } else { 5 }; $x], 4, '`do if`';
