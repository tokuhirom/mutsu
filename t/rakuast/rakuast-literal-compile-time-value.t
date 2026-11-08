use v6.e.PREVIEW;
use Test;

# A RakuAST::Literal has a compile-time value: the value it holds
# (rakudo 2026.09, #14916).

plan 13;

my @cases =
    RakuAST::IntLiteral.new(5), 5, Int,
    RakuAST::NumLiteral.new(1.5e0), 1.5e0, Num,
    RakuAST::RatLiteral.new(1/3), 1/3, Rat,
    RakuAST::StrLiteral.new("a"), "a", Str,
    RakuAST::VersionLiteral.new(v1.2), v1.2, Version;
for @cases -> $node, $value, $type {
    ok $node.has-compile-time-value, "{$node.^name}: has a compile-time value";
    is-deeply $node.compile-time-value, $value, "{$node.^name}: it is the held value";
}
my $c = RakuAST::ComplexLiteral.new(<1+2i>);
ok $c.has-compile-time-value, 'a complex literal has one';
is-deeply $c.compile-time-value, <1+2i>, 'the complex value';
is-deeply RakuAST::IntLiteral.new(7).compile-time-value, RakuAST::IntLiteral.new(7).value,
    'compile-time-value is value for a literal';
