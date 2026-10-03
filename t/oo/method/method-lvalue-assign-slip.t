use Test;

plan 5;

# Assigning a Slip to an rw accessor assigns all of its elements.
class G { has @.abscissas is rw }
class K is G {
    submethod BUILD() { my %res = a => [1, 2, 3]; self.abscissas = |%res<a> }
}
is-deeply K.new.abscissas, [1, 2, 3], 'self.attr = |%h<k> in BUILD';

class H { has @.x is rw; has $.s is rw }
my $h = H.new;
my @v = 4, 5;
$h.x = |@v;
is-deeply $h.x, [4, 5], '$obj.attr = |@v';
my @e;
$h.x = |@e;
is-deeply $h.x, [], 'an empty Slip empties it';
$h.s = |(1, 2);
is $h.s.elems, 2, 'a scalar accessor holds both elements';
$h.x = 7, 8;
is-deeply $h.x, [7, 8], 'a plain list still assigns';
