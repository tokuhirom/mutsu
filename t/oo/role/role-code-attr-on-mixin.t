use Test;

# A role's `has &!code` attribute on a `but` mixin: a write made in one
# method must be read back by another method (Kind::Subset::Parametric, #15271).

plan 3;

role R {
    has &!bb;
    method set(&b) { &!bb = &b; Nil }
    method set-bound(&b) { &!bb := &b; Nil }
    method get { &!bb }
}

my $a = Any.new but R;
$a.set({ 47 });
is $a.get.(), 47, 'assigned code attribute is read in another method';

my $b = Any.new but R;
$b.set-bound({ 48 });
is $b.get.(), 48, 'bound code attribute is read in another method';

class C does R { }
my $c = C.new;
$c.set({ 49 });
is $c.get.(), 49, 'a class composing the role behaves the same';
