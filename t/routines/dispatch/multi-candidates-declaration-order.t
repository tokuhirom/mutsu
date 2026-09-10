use Test;

# `Routine.candidates` must return a multi's candidates in DECLARATION order
# (Rakudo does). Each candidate is registered TWICE at runtime -- once by the
# forward-declaration/hoist pre-pass, once by the in-sequence pass that runs
# when execution reaches the statement -- leaving two registry rows per
# candidate with different `decl_order` stamps. `routine_candidate_subs`
# (src/runtime/methods_signature_candidates.rs) must sort by `decl_order`
# (keeping the smallest per body fingerprint) BEFORE deduping, or the result
# depends on `HashMap` bucket order and is unstable against unrelated
# statements elsewhere in the file. See
# todo/tickets/multi-candidates-declaration-order.md.

plan 6;

# Baseline: candidates come back in declaration order.
{
    multi mm(Int $x) { "int $x" }
    multi mm(Str $s) { "str $s" }
    multi mm(Rat $r) { "rat $r" }
    my @sigs = &mm.candidates.map(*.signature.gist);
    is-deeply @sigs, ['(Int $x)', '(Str $s)', '(Rat $r)'],
        'candidates() returns declaration order';
}

# Perturbation check: unrelated leading statements must not change the order.
# This is the actual regression the ticket describes -- the bug was that
# adding/removing unrelated code before the multi flipped the candidate
# order, because the surviving registry row for each candidate was picked by
# arbitrary HashMap bucket order rather than declaration order.
{
    sub twice($n) { $n * 2 }
    sub add($a, $b = 5) { $a + $b }
    sub named(:$x, :$y = 2) { "$x-$y" }
    sub empty() { 'no args' }
    multi pp(Int $x) { "int $x" }
    multi pp(Str $s) { "str $s" }
    multi pp(Rat $r) { "rat $r" }
    my @sigs = &pp.candidates.map(*.signature.gist);
    is-deeply @sigs, ['(Int $x)', '(Str $s)', '(Rat $r)'],
        'candidates() order is unaffected by unrelated leading statements';
}

# A different declared order (not type-alphabetical, not arity-based) is
# still preserved -- rules out any accidental sort-by-type-name coincidence.
{
    multi qq(Str $s) { "str $s" }
    multi qq(Int $x) { "int $x" }
    multi qq(Rat $r) { "rat $r" }
    multi qq(Bool $b) { "bool $b" }
    my @sigs = &qq.candidates.map(*.signature.gist);
    is-deeply @sigs, ['(Str $s)', '(Int $x)', '(Rat $r)', '(Bool $b)'],
        'candidates() order matches an arbitrary declaration order';
}

# The positional-dispatch consequence: calling `.candidates[N]` must invoke
# the Nth-DECLARED candidate, not whichever one a broken order happened to
# put there.
{
    multi rr(Int $x) { "int $x" }
    multi rr(Str $s) { "str $s" }
    is &rr.candidates[0].(7), 'int 7',
        'candidates[0] is the first-declared (Int) candidate';
    is &rr.candidates[1].('a'), 'str a',
        'candidates[1] is the second-declared (Str) candidate';
}

# The doc-comment lookup keys off the same positional `__mutsu_multi_index`
# the scan assigns -- verify it tracks the corrected order too by checking a
# round-trip call through each candidate reached via the (correctly ordered)
# candidate list.
{
    multi ss(Int $x) { "int $x" }
    multi ss(Str $s) { "str $s" }
    multi ss(Rat $r) { "rat $r" }
    my @results = &ss.candidates.map(-> $c {
        given $c.signature.params[0].type {
            when Int { $c.(3) }
            when Str { $c.('a') }
            when Rat { $c.(1.5) }
        }
    });
    is-deeply @results, ['int 3', 'str a', 'rat 1.5'],
        'each candidate dispatches to its own declared body';
}
