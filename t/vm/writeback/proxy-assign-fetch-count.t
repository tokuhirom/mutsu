use Test;

# An assignment into a Proxy runs only its STORE. Its FETCH runs when
# something reads the value: a parameter bind of a sigiled `$x`, or the
# assignment expression's own value (#18337).

plan 8;

my $f = 0;
my $stored;
my $p := Proxy.new(FETCH => -> $ { ++$f; 42 }, STORE => -> $, $v { $stored = $v });

$p = 2;
is $f, 0, 'a direct assignment runs no FETCH';
is $stored, 2, 'and runs the STORE';

$f = 0;
$p = 3 for ^2;
is $f, 0, 'nor does one in a loop';

sub w(Mu $x is raw) { $x = 1 }
$f = 0;
w($p);
is $f, 1, 'through a sub is raw parameter only the bind FETCHes';
is $stored, 1, 'and the STORE gets the value';

class C { method w(Mu $x is raw) { $x = 5 } }
$f = 0;
C.w($p);
is $f, 1, 'through a method is raw parameter likewise';

$f = 0;
my $r = ($p = 4);
is $f, 1, 'reading the assignment value FETCHes once';
is $r, 42, 'and that value is the FETCHed one';
