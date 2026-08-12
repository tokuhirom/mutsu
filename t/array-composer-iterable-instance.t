use v6;
use Test;

# A SINGLE user Iterable instance in a bracket array reifies through its own
# `iterator` method — `[ $csv.error_diag ]` lists CSV::Diag's six fields
# (Text::CSV t/80_diag.t "OK in list context"). Multi-element forms and
# $-variable elements keep the instance whole, matching raku.

plan 5;

class D does Iterable does Positional {
    has Int $.a = 1;
    method iterator { $[ $!a, "b", 3 ].iterator }
    method AT-POS (int $i) { $i == 0 ?? $!a !! $i == 1 ?? "b" !! 3 }
}

is [ D.new ].elems, 3, 'single Iterable instance flattens via its iterator';
is-deeply [ D.new ], [1, "b", 3], 'elements come from the user iterator';
is [ D.new, D.new ].elems, 2, 'multi-element form keeps instances whole';

my $d = D.new;
is [ $d ].elems, 1, '$-variable element stays itemized';

class Plain { has $.x = 5 }
is [ Plain.new ].elems, 1, 'non-Iterable instance stays whole';
