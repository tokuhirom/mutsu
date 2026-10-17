use Test;

# A callee's write to a caller's `$*a is rw` dynamic parameter reaches the
# bound variable, also when the program has a by-name lexical writer
# (`EVAL`) anywhere -- which boxes the program's captured lexicals into
# cells and so takes a different store path (#13701). Checked against rakudo.

plan 6;

sub assign() { $*a = 666 }
sub by-rw($*a is rw) { assign(); $*a }
my $c = 42;
is by-rw($c), 666, 'the dynamic sees the callee\'s assignment';
is $c, 666, 'and so does the bound variable';

sub incr() { $*b++ }
sub by-rw-incr($*b is rw) { incr(); incr(); $*b }
my $e = 1;
is by-rw-incr($e), 3, 'two increments in callees accumulate';
is $e, 3, 'and reach the bound variable';

sub add() { $*d += 10 }
sub by-rw-add($*d is rw) { add(); add(); $*d }
my $f = 1;
by-rw-add($f);
is $f, 21, 'a compound assignment in a callee reaches it';

sub reader() { $*g }
sub by-rw-read($*g is rw) { $*g = 'set'; reader() }
my $h = 'x';
is by-rw-read($h), 'set', 'a callee reads the routine\'s own store';

# Never called: its presence alone switched the store path.
sub uses-eval() { EVAL '1' }
