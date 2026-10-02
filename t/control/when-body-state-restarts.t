use v6.d;
use Test;

# A `when` / `default` body is a block the enclosing block re-clones on every
# execution, so its own `state` restarts on each call. Found via
# Template::HAML (DirectCodegen validates a tree recursively with a
# `state %known = ...` inside `when Statement`): the recursion left a stale
# entry in the state store and later calls saw an empty hash.

plan 6;

sub first-hit($x) {
    given $x {
        when 1 { state $e = 5; $e++ }
    }
}
is first-hit(1), 5, 'when-body state initialises';
is first-hit(1), 5, 'when-body state restarts on the next call';

sub recur($depth) {
    my $seen;
    given $depth {
        when 1 { state %known = a => 1, b => 2; $seen = %known.keys.sort.join(',') }
    }
    my $inner = recur($depth - 1) if $depth > 0;
    $seen // $inner;
}
is recur(2), 'a,b', 'when-body state set up at depth 1 of a recursion';
is recur(2), 'a,b', 'second top-level call re-initialises it';

sub dflt($x) {
    given $x {
        when 0 { 'zero' }
        default { state @a = 1, 2; @a.push(9); @a.join(',') }
    }
}
is dflt(5), '1,2,9', 'default-body state initialises';
is dflt(5), '1,2,9', 'default-body state restarts on the next call';
