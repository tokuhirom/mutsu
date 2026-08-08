use Test;
plan 10;

# ADR-0021: argument named-ness is a call-site property, not a value
# property. This pins the METHOD-call column of the divergence table --
# previously only the function-call column erased a Pair's named flavour
# at the call boundary (ContainerizePair), leaving every non-syntactic
# Pair argument to a method call misbind as named.

class D {
    multi method m(Pair $p) { 'positional:' ~ $p.key ~ '=' ~ $p.value }
    multi method m(:$a!) { 'named:a=' ~ $a }
}
my $d = D.new;

is $d.m(Pair.new('a', 1)), 'positional:a=1', 'Pair.new(...) as method arg is positional';

my $p = a => 1;
is $d.m($p), 'positional:a=1', 'variable holding a Pair is positional as a method arg';

my @l = (a => 1,);
is $d.m(@l[0]), 'positional:a=1', 'array element Pair is positional as a method arg';

sub mk() { return a => 1 }
is $d.m(mk()), 'positional:a=1', 'sub return value Pair is positional as a method arg';

is $d.m(a => 1), 'named:a=1', 'literal fat-arrow pair at the call site is still named';
is $d.m(:a(1)), 'named:a=1', 'literal colonpair at the call site is still named';

# Default constructor: a positional Pair argument must be rejected, not
# silently bound as a named constructor argument.
class Plain { has $.x; has $.y; }
my $pp = a => 1;
dies-ok { Plain.new($pp) }, 'default constructor rejects a positional Pair argument';

# Pair.new's own two-positional form must not misclassify its own args.
my $built = Pair.new('k', 'v');
is $built.key, 'k', 'Pair.new key argument unaffected by boundary erasure';
is $built.value, 'v', 'Pair.new value argument unaffected by boundary erasure';

# A parenthesised pair is positional too (method call, not just sub call).
class E {
    multi method n(Pair $p) { 'positional' }
    multi method n(:$a!) { 'named' }
}
is E.new.n((a => 1)), 'positional', 'parenthesised pair as method arg is positional';
