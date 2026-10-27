use Test;

plan 7;

# A Seq is not Positional but composes PositionalBindFailover, which a
# `Positional` parameter binds through `.cache`. The `:D` form is no exception
# (YAMLish's `multi to-yaml(Positional:D $d, ...)` over a `.map` result).
sub plain(Positional $p) { $p.elems }
sub defined-only(Positional:D $p) { $p.elems }
sub either(Positional:_ $p) { $p.elems }

my $seq = (1, 2, 3).map({ $_ * 2 });
is plain($seq), 3, 'Positional takes a Seq';
is defined-only((1, 2, 3).map({ $_ })), 3, 'Positional:D takes a Seq';
is either((1, 2).map({ $_ })), 2, 'Positional:_ takes a Seq';
is defined-only($seq), 3, 'a Seq held in a variable binds too';

my %h = :items((1, 2, 3, 4).map({ $_ }));
is defined-only(%h<items>), 4, 'a Seq stored as a Hash value binds';

throws-like { defined-only(5) }, X::TypeCheck::Binding::Parameter, 'a non-Positional is refused';

multi pick-one(Positional:D $d, Str $indent) { 'positional' }
multi pick-one(Mu:D $d, $) { 'other' }
is pick-one((1, 2).map({ $_ }), ''), 'positional', 'multi dispatch prefers the Positional:D candidate for a Seq';
