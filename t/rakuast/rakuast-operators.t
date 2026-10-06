use Test;

# Operators in RakuAST, measured on rakudo 2026.09:
#
# - `@a Z @b` / `@a X @b` is an `ApplyListInfix` over `Infix("Z")`; with an
#   operator (`Z+`, `X~`) it is over `MetaInfix::Zip` / `MetaInfix::Cross`;
#   a chain of the same operator is one flat operand list;
# - `@a R- @b` is an `ApplyInfix` over `MetaInfix::Reverse`, and an
#   `ApplyListInfix` when the base operator is list-associative (`R,`);
# - `\(1, 2, :a)` is a `Term::Capture` over an `ArgList`, `\$x` over the term;
# - `$@a` / `$%h` / `$[1, 2]` is a `Contextualizer::Item` over the term;
# - `eager EXPR` is a `StatementPrefix::Eager` over a `Statement::Expression`;
# - `1 ==> f() ==> g()` is one flat `ApplyListInfix` over `Feed("==>")`, the
#   operands in written order (`f() <== g() <== 1` lists `f()` first).
#
# The round trip is the parsed program. The tree part of this file also passes
# under `raku`; the round trip part is mutsu's.

plan 27;

sub exprs($src) { ('my ($a, $b, $c); my (@a, %h); sub foo(|) { }; ' ~ $src).AST.statements.skip(3).map(*.expression) }
sub same($src, $expected, $desc) {
    my $parsed = do { my @*LOG; EVAL($src) };
    my $round = do { my @*LOG; EVAL($src.AST) };
    is "$parsed|$round", "$expected|$expected", $desc;
}

# --- zip, cross and reverse
{
    my $z = exprs(Q[$a Z $b])[0];
    isa-ok $z, RakuAST::ApplyListInfix, '`Z` is a list infix application';
    isa-ok $z.infix, RakuAST::Infix, 'over a plain infix';
    is $z.infix.operator, 'Z', 'spelled `Z`';
    is $z.operands.elems, 2, 'with both operands';
    my $p = exprs(Q[$a Z+ $b])[0];
    isa-ok $p.infix, RakuAST::MetaInfix::Zip, '`Z+` has a zip metaoperator';
    is $p.infix.infix.operator, '+', 'over its base infix';
    my $x = exprs(Q[$a X~ $b])[0];
    isa-ok $x.infix, RakuAST::MetaInfix::Cross, '`X~` has a cross metaoperator';
    is exprs(Q[$a Z+ $b Z+ $c])[0].operands.elems, 3, 'a chain is one flat operand list';
    is exprs(Q[1, 2 Z 3, 4])[0].operands[0].infix.operator, ',', 'a comma list is an operand';
    my $r = exprs(Q[$a R- $b])[0];
    isa-ok $r, RakuAST::ApplyInfix, '`R-` is an infix application';
    isa-ok $r.infix, RakuAST::MetaInfix::Reverse, 'over a reverse metaoperator';
    is $r.infix.infix.operator, '-', 'over its base infix';
    isa-ok exprs(Q[$a R, $b])[0], RakuAST::ApplyListInfix, '`R,` is a list infix application';
    isa-ok exprs(Q[$a Rmin $b])[0], RakuAST::ApplyListInfix, 'so is the reverse of `min`';
}

# --- captures, item contexts, eager and feeds
{
    my $c = exprs(Q[\(1, 2, :a)])[0];
    isa-ok $c, RakuAST::Term::Capture, 'a capture literal';
    isa-ok $c.source, RakuAST::ArgList, 'over an argument list';
    is $c.source.args.elems, 3, 'with every argument';
    isa-ok exprs(Q[\$a])[0].source, RakuAST::Var::Lexical, 'a bare capture is over the term';
    my $i = exprs(Q[$@a])[0];
    isa-ok $i, RakuAST::Contextualizer::Item, '`$@a` is an item contextualizer';
    isa-ok $i.target, RakuAST::Var::Lexical, 'over the variable';
    isa-ok exprs(Q[$%h])[0].target, RakuAST::Var::Lexical, '`$%h` too';
    my $e = exprs(Q[eager $a])[0];
    isa-ok $e, RakuAST::StatementPrefix::Eager, '`eager` is a statement prefix';
    isa-ok $e.blorst, RakuAST::Statement::Expression, 'over a statement';
    my $f = exprs(Q[$a ==> foo() ==> foo()])[0];
    isa-ok $f, RakuAST::ApplyListInfix, 'a feed chain is a list infix application';
    isa-ok $f.infix, RakuAST::Feed, 'over a feed';
    is $f.operands.elems, 3, 'with every operand in one list';
    isa-ok exprs(Q[foo() <== $a])[0].operands[0], RakuAST::Call::Name, 'a leftwards feed lists the sink first';
}

# --- the round trip is the parsed program
my $v = 'my @a = 1, 2; my @b = 3, 4; my @c = 5, 6; ';
same $v ~ Q[(@a Z @b).join(",")], '1 3,2 4', 'a zip';
same $v ~ Q[(@a Z+ @b).join(",")], '4,6', 'a zip with an operator';
same $v ~ Q[(@a Z+ @b Z+ @c).join(",")], '9,12', 'a chain of zips';
same $v ~ Q[(@a Z @b Z @c).join(",")], '1 3 5,2 4 6', 'a plain chain';
same $v ~ Q[(@a X~ @b).join(",")], '13,14,23,24', 'a cross with an operator';
same $v ~ Q[(@a X @b X @c).elems], 8, 'a cross chain';
same $v ~ Q[(@a Z=> @b).map(*.raku).join(",")], '1 => 3,2 => 4', 'a zip over `=>`';
same Q[5 R- 3], -2, 'a reversed subtraction';
same Q[(1 R, 2 R, 3).join(",")], '3,2,1', 'a reversed comma chain';
same Q[(1, 2 Z 3, 4).join(",")], '1 3,2 4', 'comma lists as operands';
same Q[(2 R** 3)], 9, 'a reversed exponent';
same Q[((1 R+ 2) R* 5)], 15, 'a nested reverse';
same $v ~ Q[(@a Rmin @b).raku], '[1, 2]', 'a reversed `min`';
same Q[my $c = \(1, 2, :a(5)); $c.list.join(",") ~ "|" ~ $c.hash.keys], '1,2|a', 'a capture literal';
same Q[my $d = \3; $d.list.join(",")], '3', 'a bare capture';
same Q[my $e = \(); $e.elems], 0, 'an empty capture';
same Q[my @a = [1, 2]; my @b = $@a, 3; @b.elems], 2, 'an itemized array';
same Q[my %h = a => 1; my @l = $%h, 2; @l.elems], 2, 'an itemized hash';
same Q[my @i = $[1, 2], 3; @i.elems], 2, 'an itemized array composer';
same Q[(eager (1, 2, 3).map({ $_ * 2 })).join(",")], '2,4,6', '`eager`';
same Q[sub dbl(*@x) { @x.map(* * 2).join(",") }; (1, 2, 3) ==> dbl()], '2,4,6', 'a feed';
same Q[sub dbl(*@x) { @x.map(* * 2).join(",") }; dbl() <== (1, 2, 3)], '2,4,6', 'a leftwards feed';
same Q[sub dbl(*@x) { @x.map(* * 2).join(",") }; sub inc(*@x) { @x.map(* + 1) }; (1, 2) ==> inc() ==> dbl()], '4,6', 'a feed chain';
same Q[sub dbl(*@x) { @x.map(* * 2).join(",") }; sub inc(*@x) { @x.map(* + 1) }; dbl() <== inc() <== (1, 2)], '4,6', 'a leftwards feed chain';
