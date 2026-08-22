use Test;

# `function_body_needs_interpreter` used to force ANY routine whose body
# contained a `start { ... }` call in statement (or argument) position onto the
# tree-walk fallback. That blanket exclusion was dropped: ordinary module/dynamic
# single subs had already compiled `start` bodies since ADR-0019 C6e-2c, so the
# gate only still applied to *multi* candidates, protos, and builtin shadows —
# meaning byte-identical bodies compiled or tree-walked purely by declaration
# form. These cases run the historically-risky shape (a recursive routine whose
# spawned closure captures a parameter that the recursive call re-binds) through
# the *multi* and *proto* arms, which now take the compiled path. All expected
# values verified against raku.

plan 6;

# 1. Recursive multi candidates: the deep interleaving of param re-binds is the
#    exact hazard the old exclusion existed for.
{
    multi sub mfib(Int $n where * <= 1) { start { 1 } }
    multi sub mfib(Int $n) { start { await(mfib($n - 2)) + await(mfib($n - 1)) } }
    is await(mfib(10)), 89, 'recursive multi with start bodies computes fib(10)';
}

# 2. proto + multi: the proto gate also consulted the exclusion.
{
    proto sub pf($) {*}
    multi sub pf(Int $n) { start { $n == 0 ?? 'z' !! await(pf($n - 1)) ~ "-$n" } }
    is await(pf(4)), 'z-1-2-3-4', 'proto-dispatched multi with a start body stays per-invocation';
}

# 3. A Str param read AFTER the recursive await must still be this invocation's.
{
    multi sub tagm(Str $label, Int $depth) {
        start {
            $depth == 0
                ?? $label
                !! await(tagm($label ~ "-", $depth - 1)) ~ "|$label"
        }
    }
    is await(tagm("a", 3)), 'a---|a--|a-|a',
        'param read after await is per-invocation on the multi arm';
}

# 4. Sibling spawns from one invocation must not cross-talk.
{
    multi sub fanoutm(Int $n) {
        start { $n == 0 ?? 1 !! [+] await (fanoutm($n - 1), fanoutm($n - 1)) }
    }
    is await(fanoutm(4)), 16, 'sibling start invocations stay isolated on the multi arm';
}

# 5. `start` nested inside a call argument (the other position the walk saw).
{
    multi sub startarg(Int $n) { await(start { $n * 2 }) }
    is startarg(21), 42, 'start as a call argument still spawns and awaits correctly';
}

# 6. Reading the param before and after an unrelated await, on the multi arm.
{
    multi sub read-after-await(Int $n) {
        start {
            my $before = $n;
            await(Promise.in(0.01));
            "$before,{$n}"
        }
    }
    is await(read-after-await(7)), '7,7', 'param survives an await inside a multi start block';
}
