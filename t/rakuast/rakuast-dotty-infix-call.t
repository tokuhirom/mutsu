use Test;

# `TERM .meth` (whitespace before the dot) is the dotty infix `.` in rakudo
# 2026.09: `ApplyDottyInfix(left, DottyInfix::Call, Call::Method)`, and every
# `.meth` chained onto it is one too. mutsu rendered a plain postfix call.
# This file passes under both mutsu and raku.

plan 9;

sub expr(Str $src) { $src.AST.statements[*-1].expression }

my $e = expr(Q[my $a; $a .foo]);
is $e.^name, 'RakuAST::ApplyDottyInfix', 'a spaced call is a dotty infix';
is $e.infix.^name, 'RakuAST::DottyInfix::Call', 'with DottyInfix::Call';
is $e.right.^name, 'RakuAST::Call::Method', 'over the method call';
is expr(Q[my $a; $a .foo.bar]).left.^name, 'RakuAST::ApplyDottyInfix', 'a chained call is one too';
is expr(Q[my $a; $a.x .foo]).left.^name, 'RakuAST::ApplyPostfix', 'a call before the space is a postfix';
is expr(Q[my $a; $a.foo]).^name, 'RakuAST::ApplyPostfix', 'an unspaced call stays a postfix';

is EVAL(Q[(^4 .combinations(2)).elems].AST), 6, 'the call applies to the whole term';
is EVAL(Q["abc" .uc .lc].AST), 'abc', 'a chain round-trips';
is RakuAST::ApplyDottyInfix.new(
    left  => RakuAST::StrLiteral.new("x"),
    infix => RakuAST::DottyInfix::Call.new,
    right => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier("uc"))).EVAL,
    'X', 'a hand-built one';
