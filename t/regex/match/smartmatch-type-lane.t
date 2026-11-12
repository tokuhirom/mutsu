use Test;

# `$x ~~ SomeType` with a bare type name on the right is a type test that
# never topicalizes observably (#17331 answers it without the general op's
# topic machinery). Every iteration runs twice so the bare word's site memo
# is warm on the second pass.

plan 16;

role R { }
class C does R { }
class D { }
subset Small of Int where * < 10;

my $c = C.new;
for ^2 {
    ok $c ~~ R, 'an instance matches its role';
    ok $c ~~ C, 'and its class';
    nok $c ~~ D, 'not an unrelated class';
    ok $c !~~ D, 'negated';
    ok 5 ~~ Small, 'a subset runs its predicate';
    nok 50 ~~ Small, 'and rejects';
}

$_ = 'topic';
my $x = 42;
ok $x ~~ Int, 'a match with a variable on the left';
is $_, 'topic', 'the topic is left alone';
is $x, 42, 'the left variable is left alone';

{
    my constant Q = 7;
    ok 7 ~~ Q, 'a constant term on the right is matched by value';
}
