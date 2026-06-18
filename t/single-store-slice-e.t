use v6;
use Test;

# Single-store Slice E: closures capture an *upvalue snapshot* (free variables +
# shadow-meta + system names) rather than a flatten of the whole lexical env.
# These cases exercise the names the GetGlobal-based free-var scan can and cannot
# see, so they pin the capture's correctness:
#   - plain user lexicals (free vars), captured by name;
#   - `self` / attributes, read via a dedicated opcode (kept by the system-name
#     rule);
#   - `whenever`/`gather` bodies stashed in the stmt_pool (whole-env fallback via
#     `captures_env_by_name`).

plan 13;

# --- plain free-var capture ---------------------------------------------------
{
    my $count = 0;
    my $inc = { $count++ };
    $inc(); $inc(); $inc();
    is $count, 3, 'captured-and-mutated scalar accumulates';
}
{
    my $v = 0;
    my $get = { $v };
    my $set = -> $n { $v = $n };
    $set(42);
    is $get(), 42, 'sibling closures share one cell';
}
{
    sub make-counter() {
        my $c = 0;
        return (-> { $c }, -> { $c++ });
    }
    my ($g, $i) = make-counter();
    $i(); $i();
    is $g(), 2, 'escaping getter/setter factory shares state';
}
{
    my @subs;
    for 1..3 -> $x { @subs.push({ $x }) }
    is @subs.map({ $_() }).join(','), '1,2,3', 'per-iteration loop binding';
}

# --- closures dropping non-free lexicals --------------------------------------
{
    # `$noise` is in scope but not referenced by the closure: dropping it from the
    # capture must not change the result.
    my $noise = 'unused';
    my $a = 10;
    my $f = { $a * 2 };
    is $f(), 20, 'non-free lexical dropped without affecting result';
    is $noise, 'unused', 'dropped lexical still intact in the caller';
}

# --- system names read via dedicated opcodes (kept) ---------------------------
class Cache {
    has %!h;
    has @!a;
    method h-writer() { -> $k, $v { %!h{$k} = $v } }
    method a-pusher() { -> $v { @!a.push($v) } }
    method get($k) { %!h{$k} }
    method a() { @!a.List }
}
{
    my $c = Cache.new;
    my $w = $c.h-writer;
    $w('x', 1); $w('y', 2);
    is $c.get('x'), 1, 'attribute closure write (self kept) survives';
    is $c.get('y'), 2, 'second attribute closure write survives';
    my $p = $c.a-pusher;
    $p(7); $p(8);
    is $c.a.join(','), '7,8', 'attribute push closure accumulates';
}

# typed captured var keeps its constraint after escaping its scope
{
    sub make-typed() {
        my Int $t = 0;
        return -> $n { $t = $n; $t };
    }
    my $ts = make-typed();
    is $ts(5), 5, 'escaped typed closure assigns a valid value';
    my $err = 'none';
    { $ts('nope'); CATCH { default { $err = 'caught' } } }
    is $err, 'caught', 'escaped typed closure still enforces Int constraint';
}

# --- captures_env_by_name fallback (whenever / gather) ------------------------
{
    sub foo($a) {
        supply {
            whenever Supply.from-list() { LAST emit $a }
        }
    }
    is await(foo(42)), 42, 'whenever/LAST closure sees outer (whole-env fallback)';
}
{
    sub gen($base) {
        gather { take $base + $_ for 1..3 }
    }
    is gen(10).join(','), '11,12,13', 'gather body sees outer (whole-env fallback)';
}
