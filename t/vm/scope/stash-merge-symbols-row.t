use Test;

# `Stash.merge-symbols` is a row of `Stash`, found by its owner; a
# `PseudoStash` (`MY::`) declares none (ADR-11276 §9.198; checked against
# Rakudo).

plan 5;

ok GLOBAL::.can('merge-symbols'), 'a Stash can merge-symbols';
nok MY::.can('merge-symbols'), 'a PseudoStash cannot';
throws-like { MY::.merge-symbols(Mu) }, X::Method::NotFound,
    'calling it on a PseudoStash dies';
is Stash.^find_method('merge-symbols').name, 'merge-symbols', 'the method object';
my $m = 'merge-symbols';
lives-ok { GLOBALish.WHO."$m"(Mu) }, 'through a dynamic method name';
