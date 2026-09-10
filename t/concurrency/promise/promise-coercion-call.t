use Test;

plan 3;

# `Promise($supply)` — Raku's coercion protocol falls back to a method named
# after the target type on the VALUE (`$supply.Promise`) when the target has
# no applicable COERCE/new. Built-in targets whose constructors are native
# land in the user-class coercion branch, which used to go straight to
# X::Coerce::Impossible (Cro::MessageWithBody.body-blob does
# `Promise(supply { ... })`).

my $s = supply { emit 1; emit 2; };
my $p = Promise($s);
isa-ok $p, Promise, 'Promise($supply) returns a Promise';
is (await $p), 2, 'the promise keeps with the final emitted value';

throws-like { Promise("not-a-promise-source") }, Exception,
    'a value with no .Promise method still fails to coerce';
