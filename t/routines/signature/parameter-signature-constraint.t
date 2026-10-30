use Test;

plan 6;

# `Parameter.signature_constraint` (an undocumented MOP method that
# static::CLI's analysis reads, #15614) answers the undefined `Signature`
# type object for every parameter shape, as measured under rakudo.
sub f(Int $x where * > 1, ($a, $b), $c, |c) { }
my @params = &f.signature.params;

is @params.elems, 4, 'four parameters';
ok @params.map(*.signature_constraint).all === Signature, 'every constraint is the Signature type object';
nok @params.map(*.signature_constraint.defined).any, 'none of them is defined';

sub g(|c where :(Int)) { }
ok &g.signature.params[0].signature_constraint === Signature, 'a capture with a where signature too';

ok Parameter.^can('signature_constraint').elems >= 1, 'the method is discoverable';
ok &f.signature.params[1].sub_signature.defined, 'sub_signature still answers the destructure';
