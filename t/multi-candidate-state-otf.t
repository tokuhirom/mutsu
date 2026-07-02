use Test;

# A multi candidate (or a proto body) that declares `state` used to be forced
# onto the tree-walk interpreter fallback, because the OTF-compiled candidate
# was assumed to lose its `state` cell identity. It does not: the per-candidate
# `otf_compile_cache` gives each candidate a stable compiled body (stable state
# key), and the candidate's `/n`-suffixed package keeps distinct candidates'
# `state` cells apart. Removing that exclusion both drops the fallback and fixes
# a real bug: the tree-walk fallback shared one `state` cell between two
# candidates that used the same `state` variable name.

plan 6;

# Two proto'd multi candidates, each with its OWN `state $c` under the SAME name.
# The interpreter fallback wrongly shared one cell (produced 1,2,3,4,5); the
# compiled path keeps them separate.
{
    proto f(|) {*}
    multi f(Int $n) { state $c = 0; $c++; "i:$c" }
    multi f(Str $s) { state $c = 0; $c++; "s:$c" }
    is f(1),   "i:1", "Int candidate state starts at 1";
    is f(2),   "i:2", "Int candidate state increments independently";
    is f("a"), "s:1", "Str candidate state is isolated (not shared with Int)";
    is f("b"), "s:2", "Str candidate state increments independently";
}

# The caching-proto pattern: `state` lives in the proto body, `{*}` redispatches
# to the candidate only on a cache miss.
{
    my $called = "";
    proto cached($a) { state %cache; %cache{$a} //= {*} }
    multi cached($a) { $called ~= $a; $a x 2 }
    is cached("a"), "aa", "caching proto returns candidate result";
    cached("b");
    cached("a");   # cache hit: candidate must NOT run again
    is $called, "ab", "cached value did not cause an extra candidate call";
}
