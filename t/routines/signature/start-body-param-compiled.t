use Test;

# ADR-0019 C6e-2c: a `start`-containing body no longer keeps a routine on the
# interpreter arm — it compiles like any other body. The historical hazard was
# a recursive sub whose start closure captures a param: the recursive call's
# param re-bind must not clobber the value the closure keeps reading (the
# compiled caller-env merge excludes the callee's own params). These cases
# exercise that isolation through the compiled path; expected values verified
# against raku.

plan 4;

# Recursive start/await with an Int param deep enough to interleave rebinds.
{
    sub conc-fib($n) {
        start {
            $n <= 1
                ?? 1
                !! await(conc-fib($n - 2)) + await(conc-fib($n - 1))
        }
    }
    is await(conc-fib(10)), 89, 'recursive start/await fib(10) computes correctly compiled';
}

# A Str param read AFTER the recursive await must still be this invocation's.
{
    sub tag(Str $label, Int $depth) {
        start {
            $depth == 0
                ?? $label
                !! await(tag($label ~ "-", $depth - 1)) ~ "|$label"
        }
    }
    is await(tag("a", 3)), 'a---|a--|a-|a', 'param read after await is per-invocation, not the deepest rebind';
}

# Reading the param before and after an unrelated await must agree.
{
    sub read-after-await($n) {
        start {
            my $before = $n;
            await(Promise.in(0.01));
            my $after = $n;
            "$before,$after"
        }
    }
    is await(read-after-await(7)), '7,7', 'param survives an await inside the start block';
}

# Two same-depth children spawned from one invocation share the parent's $n
# without cross-talk.
{
    sub fanout($n) {
        start {
            $n == 0 ?? 1 !! [+] await (fanout($n - 1), fanout($n - 1))
        }
    }
    is await(fanout(4)), 16, 'sibling start invocations stay isolated';
}
