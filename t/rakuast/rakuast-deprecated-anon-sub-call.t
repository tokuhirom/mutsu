use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 3;

# rakudo models `is DEPRECATED((sub { "a" })())` as a trait argument holding a
# parenthesized call of an anonymous Sub (RT #120908, #14533).
my $source = Q[sub dep14533 is DEPRECATED((sub { "a" })()) { 1 }];
my $ast = $source.AST;
my $routine = $ast.statements.tail.expression;
isa-ok $routine.traits[0], RakuAST::Trait::Is, 'the deprecation stays a Trait::Is';

my $out = EVAL(Q{
    sub dep14533b is DEPRECATED((sub { "a" })()) { 7 }
    my $r = dep14533b();
    $r ~ "/" ~ Deprecation.report
}.AST);
ok $out.starts-with('7/'), 'the sub with an anonymous-sub-call message still runs after the round trip';
like $out, /'Please use a instead.'/, 'the message is the value of the anonymous sub call';
