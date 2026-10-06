use Test;

# `temp` and `let` in RakuAST, measured on rakudo 2026.09. Both are a prefix
# operator over an lvalue, so an assignment around them is the ordinary
# `ApplyInfix` and a compound one a `MetaInfix::Assign`:
#
# - `temp $x`        is `ApplyPrefix(Prefix "temp", Var::Lexical)`;
# - `temp $x = 2`    is `ApplyInfix(ApplyPrefix(...), Assignment(:item), 2)`;
# - `temp $x ~= "a"` is `ApplyInfix(ApplyPrefix(...), MetaInfix::Assign, "a")`;
# - `temp my $x = 1` puts the declaration under the prefix.
#
# The parser saves the variable in a `Stmt::Let` and assigns inside it; the
# lowering rebuilds exactly that, so the round trip restores the variable at
# scope exit like the parsed program.

plan 38;

sub exprs($src) { $src.AST.statements.map(*.expression) }
sub run($src) { my $parsed = EVAL($src); my $round = EVAL($src.AST); ($parsed, $round) }
sub same($src, $expected, $desc) {
    my ($parsed, $round) = run($src);
    is "$parsed|$round", "$expected|$expected", $desc;
}

# --- the tree
{
    my @e = exprs(Q[our $x = 1; temp $x; let $x; temp $x = 2; let $x = 3]);
    isa-ok @e[1], RakuAST::ApplyPrefix, '`temp $x` is an ApplyPrefix';
    is @e[1].prefix.operator, 'temp', 'with the `temp` prefix';
    isa-ok @e[1].operand, RakuAST::Var::Lexical, 'over the variable';
    is @e[2].prefix.operator, 'let', '`let $x` has the `let` prefix';
    isa-ok @e[3], RakuAST::ApplyInfix, '`temp $x = 2` is an ApplyInfix';
    isa-ok @e[3].left, RakuAST::ApplyPrefix, 'whose left side is the prefix';
    isa-ok @e[3].infix, RakuAST::Assignment, 'an assignment';
    like @e[3].infix.gist, /':item'/, 'which is `:item` for a scalar';
    isa-ok @e[3].right, RakuAST::IntLiteral, 'of the value';
    is @e[4].left.prefix.operator, 'let', '`let $x = 3` is the same around `let`';
}
{
    my @e = exprs(Q[our @a = 1; our %h; temp @a = 3,4; temp @a[1] = 5; temp %h{'k'} = 6]);
    unlike @e[2].infix.gist, /':item'/, 'an array assignment is not `:item`';
    isa-ok @e[2].right, RakuAST::ApplyListInfix, 'and takes the whole list';
    isa-ok @e[3].left.operand, RakuAST::ApplyPostfix, 'an element is saved through its subscript';
    isa-ok @e[3].left.operand.postfix, RakuAST::Postcircumfix::ArrayIndex, 'an array subscript';
    unlike @e[3].infix.gist, /':item'/, 'an element assignment is not `:item`';
    isa-ok @e[4].left.operand.postfix, RakuAST::Postcircumfix::HashIndex, 'a hash subscript';
}
{
    my @e = exprs(Q[our $s; temp $s ~= "a"; temp $s .= uc]);
    isa-ok @e[1].infix, RakuAST::MetaInfix::Assign, 'a compound assignment is a MetaInfix::Assign';
    isa-ok @e[1].left, RakuAST::ApplyPrefix, 'around the prefix';
    isa-ok @e[2], RakuAST::ApplyDottyInfix, '`temp $s .= uc` is an ApplyDottyInfix';
    isa-ok @e[2].left, RakuAST::ApplyPrefix, 'around the prefix';
}
{
    my @e = exprs(Q[temp my $x = 1; (temp $*CWD)]);
    isa-ok @e[0], RakuAST::ApplyPrefix, '`temp my $x = 1` is an ApplyPrefix';
    isa-ok @e[0].operand, RakuAST::VarDeclaration::Simple, 'over the declaration';
    isa-ok @e[1], RakuAST::Circumfix::Parentheses, 'a parenthesized `temp` stays a term';
    my @d = exprs(Q[temp $*CWD = 1]);
    isa-ok @d[0].left.operand, RakuAST::Var::Dynamic, 'a dynamic variable';
    my @m = Q[my $c; temp $c if True].AST.statements;
    isa-ok @m[1].expression, RakuAST::ApplyPrefix, 'under a modifier it is still the prefix';
    isa-ok @m[1].condition-modifier, RakuAST::StatementModifier::If, 'with its modifier';
}

# --- the round trip restores like the parsed program
same Q[our $x = 1; do { temp $x = 2; }; $x], 1, '`temp $x = 2` is undone at scope exit';
same Q[our $x = 1; do { temp $x = 2; my $v = $x; $v }], 2, 'and holds inside it';
same Q[our $x = 1; do { temp $x; $x = 9; }; $x], 1, 'a bare `temp` saves the value';
same Q[our @a = 1, 2; do { temp @a = 3, 4; }; @a.join(",")], '1,2', 'an array is restored';
same Q[our @a = 1, 2; do { temp @a = 3, 4; @a.join(",") }], '3,4', 'and takes the whole list';
same Q[our %h = a => 1; do { temp %h{'a'} = 5; }; %h{'a'}], 1, 'a hash element is restored';
same Q[our $s = "a"; do { temp $s ~= "b"; my $v = $s; $v }], 'ab', 'a compound assignment applies';
same Q[our $s = "a"; do { temp $s ~= "b"; }; $s], 'a', 'and is undone';
same Q[do { temp my $z = 4; $z * 2 }], 8, 'a temporized declaration';
same Q[our $c = 1; do { temp $c = 2 if False; $c }], 1, 'a false modifier does not assign';
same Q[our $c = 1; try { let $c = 3; die "no" }; $c], 1, '`let` is undone when the block fails';
same Q[our $c = 1; try { let $c = 3; 1 }; $c], 3, 'and kept when it succeeds';
