use v6;
use Test;

plan 6;

# A named sub declared in a bare block of an `EVAL` unit that runs at the
# program's top level closes over the block's later `my` like a block of the
# program itself (#16686): each call sees the previous call's write.

my @r;
EVAL '{ @r.push(foo()); @r.push(foo()); my $a; sub foo { $a++ } }';
is @r.join(','), '0,1', 'a bare block of an EVAL unit';

@r = ();
EVAL 'do { @r.push(foo()); @r.push(foo()); my $a; sub foo { $a++ } }';
is @r.join(','), '0,1', 'a do block of an EVAL unit (#16694)';

@r = ();
EVAL '{ @r.push(bar()); @r.push(bar()); my $c = 5; sub bar { $c++ } }';
is @r.join(','), '0,1', 'a declaration with an initializer that has not run yet';

@r = ();
EVAL 'if 1 { { @r.push(baz()); @r.push(baz()); my $a; sub baz { $a++ } } }';
is @r.join(','), '0,1', 'a bare block nested in an if';

@r = ();
sub w { EVAL '{ @r.push(qux()); @r.push(qux()); my $a; sub qux { $a++ } }' }
w();
is @r.join(','), '0,1', 'the same EVAL called from a routine';

@r = ();
EVAL '{ my $n = 10; sub two { $n++ }; @r.push(two()); @r.push(two()); }';
is @r.join(','), '10,11', 'a declaration before the calls';
