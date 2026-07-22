use v6;
use Test;

# Regression: `can-ok $type-object, 'method'` (and the `value_can_method`
# helper behind it) must resolve methods against a *type object*'s class MRO,
# not only against instances. Surfaced by Chemistry::Elements (TODO_dist T-039):
# `can-ok Chemistry::Elements, 'lang_str_to_column'` failed even though
# `.^can('lang_str_to_column')` returned True and the method worked.

plan 5;

class Widget {
    method frobnicate (Str:D $x --> Int:D) { 42 }
    method !secret { 1 }
}

can-ok Widget, 'frobnicate', 'can-ok on a type object finds a public method';
can-ok Widget.new, 'frobnicate', 'can-ok on an instance still works';

# Consistency with the metamodel `.^can`.
ok Widget.^can('frobnicate'), '.^can agrees (type object)';
nok Widget.^can('nonexistent'), '.^can false for a missing method';

# A missing method must still fail can-ok (guard against over-broad matching).
my $missing = Widget.^can('nonexistent').Bool;
is $missing, False, 'type object cannot do an undefined method';
