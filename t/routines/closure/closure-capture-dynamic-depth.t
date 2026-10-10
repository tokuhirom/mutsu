use Test;

# A closure created while another closure's body runs inherits that
# closure's capture, so the capture's layer count follows the dynamic nesting
# of closure creation (#12476). Names must keep resolving correctly, with the
# nearest binding winning, however deep that nesting gets.

plan 5;

class Marker { method tag { 'marker' } }
my constant LIMIT = 40;

sub combine(&f, &g) { -> $x { g(f($x)) } }

# Each generation builds its closure inside the previous one's body.
sub generation(Int $n, &prev) {
    my $own = $n;
    -> $x { my &next = combine(&prev, -> $y { $y + $own }); next($x) }
}

my &f = -> $x { $x };
&f = generation($_, &f) for 1..LIMIT;
is f(0), (1..LIMIT).sum, 'values captured at every generation resolve';

# A type name stays visible through a deeply inherited capture.
my &mk = -> { Marker.new };
for 1..LIMIT -> $i {
    my &inner = &mk;
    &mk = -> { my $o = inner(); $o }
}
is mk().tag, 'marker', 'a type name resolves through many inherited layers';

# The nearest binding of a lexical wins over an enclosing one.
my $v = 'outer';
my &get = -> { $v };
for 1..LIMIT -> $i {
    my $v = "level $i";
    my &prev = &get;
    &get = -> { $v ~ '/' ~ prev().split('/')[0] };
}
is get().split('/')[0], "level $(LIMIT)", 'the nearest lexical wins at the deepest level';

# Mutation through an inherited capture stays shared.
my $count = 0;
my &bump = -> { $count++ };
for 1..LIMIT { my &p = &bump; &bump = -> { p(); $count++ } }
bump();
is $count, LIMIT + 1, 'writes through a deep chain reach the shared variable';

# Dynamics keep resolving.
my $*dyn = 'dynamic';
my &d = -> { $*dyn };
for 1..LIMIT { my &p = &d; &d = -> { p() } }
is d(), 'dynamic', 'a dynamic variable resolves through a deep chain';
