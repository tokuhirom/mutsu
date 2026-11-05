use Test;

# #14916 (accessor survey): `RakuAST::ArgList`'s `arity`, `has-args` and
# `arg-at-pos`, as on rakudo 2026.09. A negative position counts from the end
# and a position past either end answers the `Mu` type object.

plan 12;

sub n($v) { RakuAST::IntLiteral.new($v) }

my $empty = RakuAST::ArgList.new;
my $two = RakuAST::ArgList.new(n(1), n(2));

is $empty.arity, 0, 'an empty list has arity 0';
is $two.arity, 2, 'two arguments';
isa-ok $two.arity, Int, 'arity is an Int';
is-deeply $empty.has-args, False, 'an empty list has no args';
is-deeply $two.has-args, True, 'a list with arguments has args';

is $two.arg-at-pos(0).raku, 'RakuAST::IntLiteral.new(1)', 'the first argument';
is $two.arg-at-pos(1).raku, 'RakuAST::IntLiteral.new(2)', 'the second argument';
is $two.arg-at-pos(-1).raku, 'RakuAST::IntLiteral.new(2)', 'a negative position counts from the end';
ok $two.arg-at-pos(5) === Mu, 'past the end is Mu';
ok $empty.arg-at-pos(0) === Mu, 'an empty list has no first argument';

# A parsed call's argument list answers the same.
my $args = 'say(1, 2, 3)'.AST.statements[0].expression.args;
is $args.arity, 3, 'a parsed call has its arguments';
is $args.arg-at-pos(2).raku, 'RakuAST::IntLiteral.new(3)', 'and indexes them';
