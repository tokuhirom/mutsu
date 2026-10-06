use Test;

# Operators in RakuAST, measured on rakudo 2026.09:
#
# - `@a Z @b` / `@a X @b` is an `ApplyListInfix` over `Infix("Z")`; with an
#   operator (`Z+`, `X~`) it is over `MetaInfix::Zip` / `MetaInfix::Cross`;
#   a chain of the same operator is one flat operand list;
# - `@a R- @b` is an `ApplyInfix` over `MetaInfix::Reverse`, and an
#   `ApplyListInfix` when the base operator is list-associative (`R,`).
#
# The round trip is the parsed program. The tree part of this file also passes
# under `raku`; the round trip part is mutsu's.

plan 27;

sub exprs($src) { ('my ($a, $b, $c); ' ~ $src).AST.statements.skip(1).map(*.expression) }
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
