use Test;

# A parenthesized zero-argument call (`f()`) of a zero-parameter routine binds
# on the positional light path, or runs its TRIR chunk, from the second call on
# (#18080). The parser's callsite-line marker the call carries must not change
# any answer: the routine sees no argument, an over-supplied call still fails
# as the general binder words it, and a test assertion still reports its line.
plan 9;

sub e0() { 42 }
my $sum = 0;
$sum += e0() for ^5;
is $sum, 210, 'a zero-parameter routine called repeatedly';

my $n = 0;
sub bump() { $n++ }
bump() for ^4;
is $n, 4, 'a zero-argument call writing a program-scope variable';

sub early() { return 'r'; 'not reached' }
is (early() for ^3).join(','), 'r,r,r', 'return from a zero-argument call';

sub no-sig { @_.elems }
is no-sig(), 0, 'a routine without a signature sees no argument';

sub typed-ret(--> Int) { 7 }
is (typed-ret() for ^3).join(','), '7,7,7', 'a zero-parameter routine with a return type';

my @extra = 1;
e0() for ^3;
throws-like { e0(|@extra) }, Exception,
    message => /'Too many positionals'/, 'an over-supplied call after the cache is warm';

sub inner-call() { e0() + e0() }
is (inner-call() for ^3).join(','), '84,84,84', 'a zero-argument call inside one';

sub cnt() { state $c = 0; ++$c }
is (cnt() for ^3).join(','), '1,2,3', 'a zero-parameter routine with state';

# The failure of a test assertion called with no arguments names the line of
# the call, read off the marker, also once the call takes the light path.
my $code = q:to/END/;
use Test;
sub where-am-i() is test-assertion { flunk 'reported' }
for ^3 {
    where-am-i();
}
END
my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
is $proc.err.slurp(:close).comb(/'line ' \d+/).unique.join(','), 'line 4',
    'a test assertion reports the line of its zero-argument call';
