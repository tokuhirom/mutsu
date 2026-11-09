use v6;
use Test;

# A hyper method call over a nested list whose elements are array-slot cells
# (`($x[0], $x[1])`, elements read off a Match's capture array) dispatches on
# what each cell holds: `».made` / `».ast` / `».from` of a Match used to die
# "No such method 'made' for invocant of type 'G'" (Net::NetRC's actions).
plan 9;

my grammar G { token TOP { <a>+ }; token a { 'a' } }
my @seen;
my class Act {
    method TOP($/) {
        my $x = $/<a>;
        @seen.push: (($x[0], $x[1]),)».made;
        @seen.push: (($x[0], $x[1]),)».ast;
        @seen.push: (($x[0], $x[1]),)».from;
        @seen.push: (($x[0], $x[1]),)».Str;
        @seen.push: (($/<a>[0],),)».made;
        @seen.push: (($/<a>[0], $/<a>[1]),)».made;
        make 1;
    }
    method a($/) { make 5 }
}
my $m = G.parse('aaa', :actions(Act.new));
is $m.made, 1, 'the grammar action ran';
is-deeply @seen[0], ($(5, 5),), '».made over a nested list of slot cells';
is-deeply @seen[1], ($(5, 5),), '».ast';
is-deeply @seen[2], ($(0, 1),), '».from';
is-deeply @seen[3], ($("a", "a"),), '».Str (unchanged)';
is-deeply @seen[4], ($(5,),), 'a one-element nested list read inline';
is-deeply @seen[5], ($(5, 5),), 'inline captures in a nested list';

# In-place leaf mutation through the cell is still written back.
my @b = 1, 2, 3;
my $c = @b;
(($c[0], $c[1]),)>>++;
is-deeply @b, [2, 3, 3], 'a mutating hyper call writes back through the cell';
my @n = [1, 2], [3, 4];
@n>>++;
is-deeply @n, [[2, 3], [4, 5]], 'nested arrays are still mutated in place';
