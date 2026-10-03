use Test;

plan 4;

# STORE on an `is Hash` instance replaces its backing hash in place, so a
# call through `self` inside a method (or `self = ...`) is seen by the
# caller, not just by the name the call happened to be written back to.
class H is Hash {
    method fill  { self.STORE((z => 7,)) }
    method reset { self = (x => 9) }
}

my %g := H.new;
%g<a> = 1;
%g.fill;
is-deeply %g.pairs.List, (z => 7,), 'self.STORE inside a method';

%g.reset;
is-deeply %g.pairs.List, (x => 9,), 'self = ... inside a method';

my $h = H.new;
$h.fill;
is-deeply $h.pairs.List, (z => 7,), 'through a scalar holding the instance';

my %k := H.new;
%k.STORE((y => 2,));
is-deeply %k.pairs.List, (y => 2,), 'a direct STORE call still works';
