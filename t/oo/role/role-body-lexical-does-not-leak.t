use Test;

# A role body's `my` lexicals belong to the composing class's static store;
# composing the role must not leave them bound in the composing scope, where a
# later, unrelated `$n` would find them (ADR-16595 slice 5, #16600).

plan 5;

{
    role R { my $n = 10; method get { $n } }
    class A does R {}
    nok (try EVAL '$n').defined, 'the role-body lexical is not visible after composition';
    is A.get, 10, 'the composed method still reads its role-body lexical';
}

{
    my $n = 'outer';
    role R2 { my $n = 0; method inc { ++$n } }
    class B does R2 {}
    is $n, 'outer', 'composing a role does not overwrite a same-named outer lexical';
    is B.inc, 1, 'the composed method uses the role-body lexical';
    is B.inc, 2, 'and keeps it across calls';
}
