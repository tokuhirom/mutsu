use Test;

# Pin for `Interpreter::gather_compile_cache`: a `gather` body's bytecode is now
# compiled once per body and reused, instead of running the whole compiler on
# every evaluation of the `gather` expression. Nothing observable may change, and
# in particular each gather instance must still be independent.

plan 15;

# --- A gather in a loop: each instance is independent ------------------------
my @sums;
for 1, 2, 3 -> $i {
    my @g = gather {
        take $i;
        take $i * 10;
    };
    @sums.push: @g.join('/');
}
is @sums.join(','), '1/10,2/20,3/30', 'each gather instance captures its own lexical';

# The same literal evaluated twice from a routine.
sub two($n) { return gather { take $n; take $n + 1 } }
is two(5).join(','), '5,6', 'first call';
is two(9).join(','), '9,10', 'second call is independent';

# --- Laziness is preserved ---------------------------------------------------
my $pulled = 0;
my $lazy = gather {
    for 1 .. 100 {
        $pulled++;
        take $_;
    }
};
is $lazy[0], 1, 'first element of a lazy gather';
is $lazy[1], 2, 'second element';
ok $pulled < 100, 'the gather did not eagerly run its whole body';
is $lazy[4], 5, 'later elements still come out in order';

# --- A gather whose body declares a routine ----------------------------------
# Such a body compiles through a wrapping `Stmt::Block` so the declaration stays
# lexical to it; that decision is part of the cached computation.
my @r1 = gather { sub helper() { 'h1' }; take helper(); };
is @r1.join(','), 'h1', 'a gather body may declare a routine';
my @r2 = gather { sub helper() { 'h2' }; take helper(); };
is @r2.join(','), 'h2', 'a sibling gather declaring the same name does not collide';
nok defined(::('helper')), 'the routine stayed lexical to its gather body';

# --- Nested and repeated gathers ---------------------------------------------
my @nested = gather {
    for 1, 2 -> $x {
        take [gather { take $x; take $x * 2 }].join('-');
    }
};
is @nested.join(','), '1-2,2-4', 'a gather nested inside a gather';

# --- Control flow inside a gather body ---------------------------------------
my @ctl = gather {
    for 1 .. 5 {
        next if $_ %% 2;
        last if $_ > 3;
        take $_;
    }
};
is @ctl.join(','), '1,3', 'next/last inside a gather body';

# --- Empty and single-element gathers ----------------------------------------
is gather { }.elems, 0, 'an empty gather yields nothing';
is gather { take 42 }.join(','), '42', 'a one-take gather';

# --- A cached gather can be read twice ---------------------------------------
my $g = (gather { take 'a'; take 'b' }).cache;
is "{$g.join(',')}|{$g.join(',')}", 'a,b|a,b',
    'reading a cached gather twice yields the same values';
