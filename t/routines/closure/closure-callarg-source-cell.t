use Test;

# A `:=`-bound scalar (`my $path := EXPR`) is an immutable binding, so its
# captured snapshot in a closure can never go stale — even when the local is
# also handed to a call as an argument (which normally vetoes it from
# `authoritative_free_vars` for fear of an `is rw` writeback). Vouching for it
# lets a closure that carries it into a FOREIGN frame with a same-named parameter
# still resolve its own lexical binding, instead of dynamically picking up the
# callee's parameter. Regression: zef's Zef::Extract.extract, where
# `my $path := $candi.uri` reached `self!extractors($path)` and was captured by
# `.map(-> $ex { lock-file-protect(IO() $path, -> { ... $ex.extract($path) }) })`
# and mis-resolved to lock-file-protect's own `$path` parameter (the `.lock`
# file), so tar tried to untar the lock file. This is a by-value overwrite-install
# (no boxing), so scalars holding Set/Bag/Mix/Pair or an `is default` trait are
# untouched.

plan 6;

# 1) The core misresolution: a `:=`-bound $path captured through map -> block,
#    must keep its lexical value, not the callee's same-named parameter.
{
    sub other1($p) { 1 }
    sub callee1($path, &code) { code() }
    sub extract1($candi) {
        my $path := $candi;
        other1($path);
        <A B>.map(-> $b { callee1("$path.lock", -> { "$b:$path" }) })
    }
    is extract1("REAL").join(","), "A:REAL,B:REAL",
        "captured :=-bound call-arg-source keeps lexical value, not callee param";
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

# 3) The value is a reference type (an Instance): by-value overwrite-install works
#    without boxing (the box path skips reference types, so the old cell approach
#    could not fix this).
{
    class Cand3 { has $.uri }
    class Ex3 { method extract($archive) { "GOT:" ~ $archive.basename } }
    sub lock3($path, &code) { code() }
    sub extract3($candi) {
        my $path := $candi.uri;
        my Ex3 @ex = Ex3.new;
        @ex.map(-> $e {
            lock3("$path.lock", -> { my $t = start { $e.extract($path) }; await $t; $t.result })
        }).head;
    }
    is extract3(Cand3.new(uri => "/tmp/REAL.tar.gz".IO)), "GOT:REAL.tar.gz",
        "Instance-valued :=-bound path resolves lexically (no boxing needed)";
}

# 4) The veto is still honored for `=`-assigned scalars: a genuine `is rw`
#    writeback after capture must be observed (by-value would go stale, so those
#    must NOT be made authoritative).
{
    sub mutate4($x is rw) { $x = "NEW" }
    sub f4() {
        my $x = "orig";
        my $c = -> { $x };
        mutate4($x);
        $c();
    }
    is f4(), "NEW", "an =-assigned rw-writeback is still seen (veto preserved)";
}

# 5) grep block variant (separate inline recompile path).
{
    sub other5($p) { 1 }
    sub callee5($path, &code) { code() }
    sub extract5($candi) {
        my $path := $candi;
        other5($path);
        <A B>.grep(-> $b { callee5("$path.lock", -> { "$b:$path" eq "A:REAL" }) })
    }
    is extract5("REAL").join(","), "A",
        "grep block capture of a :=-bound call-arg-source resolves lexically";
}

# 6) No over-vouch: a same-named local in a sibling frame is untouched.
{
    sub other6($p) { 1 }
    sub callee6($path, &code) { code() }
    sub extract6($candi) {
        my $path := $candi;
        other6($path);
        callee6("shadow", -> { $path });
    }
    is extract6("live"), "live",
        "closure invoked from a same-named foreign frame keeps its own binding";
}
