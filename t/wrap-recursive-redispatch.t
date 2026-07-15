use v6;
use Test;

plan 6;

# A recursive named call from inside a wrapped sub's original body must
# re-enter the wrap chain (advent2011-day04: the is Cached memoization idiom).

my @seen;
sub fib($x) { $x <= 1 ?? 1 !! fib($x - 1) + fib($x - 2) }
&fib.wrap(-> $arg { @seen.push($arg); callwith($arg) });
is fib(3), 3, 'wrapped recursive fib result';
is-deeply @seen, [3, 2, 1, 0, 1], 'every recursive call went through the wrapper';

# The full is Cached trait idiom
our %cache;
multi sub trait_mod:<is>(Routine $r, :$Cached!) {
    $r.wrap(-> $arg {
        %cache{$arg}:exists
            ?? %cache{$arg}
            !! (%cache{$arg} = callwith($arg))
        }
    );
}
sub cfib($x) is Cached {
    $x <= 1 ?? 1 !! cfib($x - 1) + cfib($x - 2);
}
is cfib(10), 89, 'cached fib result';
is-deeply %cache, {1 => 1, 0 => 1, 2 => 2, 3 => 3,
                   4 => 5, 5 => 8, 6 => 13, 7 => 21,
                   8 => 34, 9 => 55, 10 => 89}, 'every recursion level was cached';

# A nextcallee-returned wrappee runs directly — calling it must NOT re-enter
# the wrap chain (S12-methods/defer-call.t GH#4465 idiom).
{
    use soft;
    my @xs;
    sub foo { push @xs, 'X' };
    &foo.wrap: { my &wrappee = nextcallee; wrappee; wrappee; wrappee; };
    foo;
    is-deeply @xs, ['X', 'X', 'X'], 'nextcallee wrappee called multiple times';
}

# callsame in a wrapper exhausts the chain instead of looping
{
    use soft;
    my @xs;
    sub bar { push @xs, 'Y' };
    &bar.wrap: { callsame; callsame };
    bar;
    is-deeply @xs, ['Y'], 'callsame exhausts the wrap chain';
}
