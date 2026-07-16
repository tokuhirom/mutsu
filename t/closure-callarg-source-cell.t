use Test;

# A free variable that (a) is handed to a call as an argument (so the
# compile-time vouch analysis conservatively refuses to make it authoritative,
# since an `is rw` param might write it back) and (b) is captured by a nested
# closure must NOT dynamically resolve to a same-named parameter of the callee
# that eventually invokes the closure. The sound fix cell-ifies such a local
# (ContainerRef), which is overwrite-installed at closure entry and live-tracks
# any genuine rw writeback. Regression: zef's Zef::Extract.extract, where
# `my $path := $candi.uri` reached `self!extractors($path)` and was captured by
# `.map(-> $ex { lock-file-protect(IO() $path, -> { ... $ex.extract($path) }) })`
# and mis-resolved to lock-file-protect's own `$path` param (the `.lock` file).

plan 6;

# 1) The core misresolution: $path captured through map -> block, must keep its
#    lexical value, not the callee's same-named parameter.
{
    sub other1($p) { 1 }
    sub callee1($path, &code) { code() }
    sub extract1($candi) {
        my $path := $candi;
        other1($path);
        <A B>.map(-> $b { callee1("$path.lock", -> { "$b:$path" }) })
    }
    is extract1("REAL").join(","), "A:REAL,B:REAL",
        "captured call-arg-source keeps lexical value, not callee param";
}

# 2) Three-level nesting (map -> block -> start), as in zef.
{
    sub other2($p) { 1 }
    sub callee2($path, &code) { code() }
    sub extract2($candi) {
        my $path := $candi;
        other2($path);
        <A B>.map(-> $b { callee2("$path.lock", -> { await start { "$b:$path" } }) })
    }
    is extract2("REAL").join(","), "A:REAL,B:REAL",
        "three-level nested capture (map/block/start) resolves lexically";
}

# 3) A genuine rw writeback in the same frame must still be observed through the
#    cell (soundness in the other direction — the cell live-tracks).
{
    sub mutate($x is rw) { $x = "MUT" }
    sub f3() {
        my $x = "orig";
        my $c = -> { $x };
        mutate($x);
        $c();
    }
    is f3(), "MUT", "cell live-tracks a real rw writeback after capture";
}

# 4) grep block variant (separate inline recompile path).
{
    sub other4($p) { 1 }
    sub callee4($path, &code) { code() }
    sub extract4($candi) {
        my $path = $candi;
        other4($path);
        <A B>.grep(-> $b { callee4("$path.lock", -> { "$b:$path" eq "A:REAL" }) })
    }
    is extract4("REAL").join(","), "A",
        "grep block capture of a call-arg-source resolves lexically";
}

# 5) Identity: a plain scalar captured through this path is not spuriously
#    aliased to a fresh container (=:= against itself still holds).
{
    sub g5($p) { 1 }
    sub run5($path, &code) { code() }
    sub h5() {
        my $v = 42;
        g5($v);
        my $seen;
        run5("x", -> { $seen := $v });
        $seen == 42;
    }
    ok h5(), "captured call-arg-source scalar reads its value correctly";
}

# 6) No over-capture: an unrelated same-named local in a sibling frame is
#    untouched.
{
    sub other6($p) { 1 }
    sub callee6($path, &code) { code() }
    sub extract6($candi) {
        my $path = $candi;
        other6($path);
        callee6("shadow", -> { $path });
    }
    is extract6("live"), "live",
        "closure invoked from a same-named foreign frame keeps its own binding";
}
