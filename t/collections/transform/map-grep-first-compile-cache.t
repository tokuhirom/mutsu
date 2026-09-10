use v6;
use Test;

# `Interpreter::compile_loop_block_cached` (resolution_map_grep.rs) caches the
# inline-loop fast path's compile of a .map/.grep/.first callback, keyed by
# the closure literal's `compiled_code` Arc pointer identity, so a block
# declared *inside* an outer loop reuses its compile across iterations
# instead of recompiling a fresh `SubData` every time.

plan 4;

# Each outer iteration instantiates a fresh SubData for the SAME source
# closure literal; the captured `$round` must still be read correctly from
# each fresh instance's own env, not from a stale cached one.
{
    my @seen;
    for ^4 -> $round {
        @seen.push((1, 2, 3).map({ $_ + $round }).List);
    }
    is @seen.raku, [(1, 2, 3), (2, 3, 4), (3, 4, 5), (4, 5, 6)].raku,
        'map in a loop reuses the compiled block correctly across fresh SubData instances';
}

{
    my @seen;
    for ^4 -> $round {
        @seen.push((1, 2, 3, 4).grep({ $_ + $round > 3 }).List);
    }
    is @seen.raku, [(4,), (3, 4), (2, 3, 4), (1, 2, 3, 4)].raku,
        'grep in a loop reuses the compiled block correctly across fresh SubData instances';
}

{
    my @seen;
    for ^4 -> $round {
        @seen.push((1, 2, 3, 4).first({ $_ + $round > 3 }));
    }
    is @seen.raku, [4, 3, 2, 1].raku,
        'first in a loop reuses the compiled block correctly across fresh SubData instances';
}

# A closure literal's `compiled_code` Arc may be dropped once its SubData
# goes out of scope (true for a dynamically-built EVAL closure, which is
# never retained in an enclosing scope's `closure_compiled_codes`); a cache
# keyed by bare pointer address would risk a later, unrelated allocation
# reusing that freed address and returning a stale cache hit. This loop
# builds a DIFFERENT map/grep block via EVAL on every iteration to stress
# that path (see `t/rakuast-eval-block-arg.t` for the chained-call case that
# caught this during development).
{
    use MONKEY-SEE-NO-EVAL;
    my @seen;
    for ^6 -> $n {
        my $src = '(1..5).map({ $_ + ' ~ $n ~ ' }).grep({ $_ %% 2 }).join(\',\')';
        @seen.push(EVAL($src));
    }
    is @seen.join('|'), '2,4|2,4,6|4,6|4,6,8|6,8|6,8,10',
        'repeated EVAL of distinct map/grep blocks never collides via compile-cache pointer reuse';
}
