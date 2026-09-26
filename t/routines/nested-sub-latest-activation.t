use v6;
use Test;

# A named sub declared inside a routine is one static code object whose outer
# is the most recent activation of that routine (rakudo's capturelex). Called
# from code that captured none of its bindings -- through an `our` alias, an
# `is export`, a `multi` candidate registry -- it must still see that
# activation's lexicals, not its caller's. Found through the `Exportable`
# distribution (Color::DirColors).

plan 7;

sub f($v) { my $x = $v; our sub g { "g $x" } }
f(5);
is g(), 'g 5', 'an `our` sub sees the declaring activation';
f(7);
is g(), 'g 7', 'and the latest activation after a second call';

sub reg-maker {
    my %seen;
    multi sub note-it(Int $k) { %seen{$k} = 'int' }
    multi sub note-it(Str $k) { %seen{$k} = 'str' }
    our &noter = &note-it;
    -> { %seen }
}
my $get = reg-maker();
our &noter;
noter(1);
noter('a');
is $get().sort.map({ "{.key}={.value}" }).join(','), '1=int,a=str',
    'multi candidates write the same hash the returned closure reads';

# A closure assigns to an enclosing `my &k`: a Callable container, not a
# routine name.
my &k;
sub set-k { &k = -> { 'from set-k' } }
set-k();
is k(), 'from set-k', '`&k = ...` inside a sub assigns the outer container';
my $f = sub { &k := -> { 'bound' } };
$f();
is k(), 'bound', '`&k := ...` inside a closure binds the outer container';

sub named { 1 }
throws-like { &named = -> { 2 } }, Exception, 'a routine name is still read-only';

sub outer-with-hash {
    my %h;
    sub add($key) { %h{$key} = True }
    &add;
}
my &adder = outer-with-hash();
adder('x');
pass 'a routine-nested sub called after its routine returned does not die';
