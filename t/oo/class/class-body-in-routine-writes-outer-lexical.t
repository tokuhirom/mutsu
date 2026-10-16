use Test;

# A `my class` declared inside a routine or a closure runs its body statements
# when the routine runs. A variable of an outer scope that the body writes is a
# free variable of the routine, so the routine captures its cell and the write
# reaches the declaring block without a frame's return merge carrying it
# (ADR-12529 §3, slice 35). Checked against rakudo.

plan 5;

sub run(&c) { c() }

my $log = '';
sub f { my class K { $log ~= 'body;'; method m { 'meth' } } }
f();
f();
is $log, 'body;body;', 'a scalar the class body appends to, on each call';

my @seen;
sub g { my class L { @seen.push('L') } }
g();
is-deeply @seen, ['L'], 'an array the class body pushes to';

my %count;
sub h { my class M { %count<M>++ } }
h();
h();
is %count<M>, 2, 'a hash element the class body increments';

my $n = 0;
run { my class N { $n++ } };
is $n, 1, 'a class declared in a closure another routine runs';

my $read = 'outer';
sub k { my class P { $read ~= '!' } }
k();
is $read, 'outer!', 'a class body writing a variable it also reads';
