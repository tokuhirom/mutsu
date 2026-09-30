use Test;

# ADR-0134 slice 2 (#10329): a BEGIN nested in a scope that declares a routine
# ahead of it is still lifted to BEGIN time. It runs once, before the unit's
# run time, whether or not the enclosing scope ever runs, and it may call the
# routine, which at that time holds the static state of the variables it closes
# over.

plan 15;

my @log;

sub after-helper { sub helper { 1 }; BEGIN @log.push('after-helper') }
is @log.join(','), 'after-helper',
    'a BEGIN after a routine declared in its scope runs though the scope never does';

my $upper;
sub calls-helper { sub my-uc($x) { $x.uc }; BEGIN { $upper = my-uc 'Ab' } }
is $upper, 'AB', 'a BEGIN calls a routine declared ahead of it';

my $closed;
sub closes-over { my $x; BEGIN $x = 5; sub get { $x }; BEGIN { $closed = get() } }
is $closed, 5, 'the routine it calls sees the static value of a variable it closes over';

my $chained;
sub transitive { sub one { 2 }; sub two { one() + 1 }; BEGIN $chained = two() }
is $chained, 3, 'a routine the BEGIN calls may call another declared ahead of it';

my $in-loop;
sub in-loop { sub helper { 7 }; for 1 { BEGIN $in-loop = helper() } }
is $in-loop, 7, 'a BEGIN in an inner block sees a routine of the enclosing scope';

my $count;
sub counted { sub helper { 1 }; BEGIN $count++ }
counted(); counted();
is $count, 1, 'such a BEGIN does not run again when its scope does';

sub value-form { sub helper { 9 }; my $v = BEGIN helper(); $v }
is value-form(), 9, 'a value-form BEGIN may call a routine declared ahead of it';

my $param-seen;
sub with-param($p) { sub helper { 'h' }; BEGIN { $param-seen = helper() ~ ($p.defined ?? 'D' !! 'U') } }
is $param-seen, 'hU', 'a parameter the BEGIN reads is unbound at BEGIN time';

sub helper { 'outer' }
my $shadowed;
sub shadows { sub helper { 'inner' }; BEGIN $shadowed = helper() }
is $shadowed, 'inner', 'a routine declared in the scope shadows a unit-level one';

sub both { my $x; sub helper { 'h' }; BEGIN $x = helper(); $x }
is both() ~ both(), 'hh', 'the scope starts from what the BEGIN stored, and keeps its own routine';

my $deep;
sub deep { sub helper($n) { $n * 2 }; { my $k; BEGIN $k = 4; BEGIN { $deep = helper($k) } } }
is $deep, 8, 'a BEGIN reads an inner variable and a routine of an outer scope together';

my $bare;
{ sub my-uc2($x) { $x.uc }; BEGIN { $bare = my-uc2 'Ab' } }
is $bare, 'AB', 'a BEGIN in a bare block calls the routine declared ahead of it';

my $ref;
sub by-ref { sub helper { 'r' }; BEGIN { $ref = &helper.() } }
is $ref, 'r', 'a BEGIN may reach the routine as &helper';

my @order;
sub first-begin { sub helper { 1 }; BEGIN @order.push('first') }
sub second-begin { BEGIN @order.push('second') }
is @order.join(','), 'first,second', 'a BEGIN after a declared routine does not hold back later ones';

my $run-time;
sub run-time-call { sub helper { 'run' }; helper() }
$run-time = run-time-call();
is $run-time, 'run', 'the routine is still declared when its scope runs';
