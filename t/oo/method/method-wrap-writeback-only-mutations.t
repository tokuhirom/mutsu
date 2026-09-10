use OO::Monitors;
use Test;

plan 3;

# A method wrapper's closure captures the lexicals that were live where it was
# created — and once `reflective_name_access_possible()` has latched, that
# capture is the WHOLE env. When the wrapped method returns, only the names the
# wrapper actually ASSIGNED may reach the caller: a merely-captured name must
# not be republished over an unrelated caller lexical that shares it.
#
# `monitor` is the real-world source of these wrappers (OO::Monitors wraps every
# monitor method), and it is how HTTP::HPACK's block-local `my int $i` used to
# land on the `$i` of a `for` loop several frames up in Cro's HTTP client.

# Latch `reflective_name_access_possible()`, which is what makes closure capture
# snapshot the whole env by name. Any `EVAL` / `::()` / pseudo-stash use in the
# program does this, and Cro's dependency tree certainly does — without it the
# wrapper captures only its free variables and the bug cannot show.
my $latch = 1;
my $latched = ::('$latch');

# Live where the monitor's method wrappers are created, and never assigned by
# them.
my $shared = -1;

monitor M {
    method m() { 42 }
}

sub uses-the-same-name() {
    my $shared = 7;
    my $got = M.new.m();
    ($got, $shared)
}

my ($got, $after) = uses-the-same-name();
is $got, 42, 'the wrapped monitor method still returns its value';
is $after, 7, "a merely-captured lexical does not overwrite the caller's same-named one";

# A lexical the wrapper really does mutate must still be seen by the caller.
my $counter = 0;
class D { method n() { 'x' } }
D.^find_method('n').wrap(-> |c { $counter++; callsame() });
D.new.n();
D.new.n();
is $counter, 2, 'a lexical a wrapper really mutates still reaches the caller';
