use Test;

# `StateVarInit` runs on every entry to a `state` declaration; after the first
# entry it only re-binds the stored cell (#14788). These pin what that re-bind
# must keep doing: one cell per declaration, per-closure and per-iteration
# scoping, and the variable visible to name-based readers.

plan 14;

sub counter { state $n = 0; ++$n }
counter() for ^3;
is counter(), 4, 'a state scalar keeps its value across calls';

sub anon { $ = 41; }
is anon(), 41, 'anonymous state scalar assigned in a sub';
is-deeply (^3).map({ anon() }).List, (41, 41, 41), 'and again on later calls';

sub bump { state $x = 10; $x += 5 }
bump();
is bump(), 20, 'a compound assignment to a state scalar accumulates';

# Two declarations in one routine are two cells.
sub pair-of { state $a = 1; state $b = 100; ($a++, $b--) }
pair-of();
is-deeply pair-of(), (2, 99), 'two state scalars in one routine stay separate';

# Each closure made from a factory owns its state.
sub make { return sub { state $c = 0; ++$c } }
my &c1 = make();
my &c2 = make();
c1() for ^4;
is c1(), 5, 'closure one counts on';
is c2(), 1, 'closure two has its own state';

# A state variable inside a loop body restarts with each execution of the
# enclosing statement, and keeps its value across the loop's iterations.
my @seen;
for ^2 -> $round {
    for ^3 { state $k = 0; @seen.push($k++) }
}
is-deeply @seen, [0, 1, 2, 0, 1, 2], 'a loop-body state restarts with its enclosing statement';

# Name-based readers see the cell.
sub reader { state $v = 7; $v++; EVAL '$v' }
reader();
is reader(), 9, 'EVAL sees the state variable by name';

# Typed state scalars keep checking their constraint.
sub typed { state Int $t = 1; $t += 1 }
typed();
is typed(), 3, 'a typed state scalar accumulates';
sub typed-bad { state Int $t = 1; $t = 'x' }
throws-like { typed-bad() }, X::TypeCheck::Assignment, 'a typed state scalar still checks its constraint';

# Arrays and hashes.
sub agg { state @a; state %h; @a.push(@a.elems); %h{@a.elems} = 1; (@a.elems, %h.elems) }
agg();
is-deeply agg(), (2, 2), 'state aggregates accumulate';

# Recursion: one declaration, one cell.
sub rec($d) { state $calls = 0; $calls++; rec($d - 1) if $d > 0; $calls }
is rec(3), 4, 'recursive calls share one state cell';

# A state variable assigned in a hot loop (the roast regression shape).
sub foo () { $ = 42 }
my $last;
for ^1000 { $last = foo }
is $last, 42, 'anonymous state assignment in a loop';
