use v6;
use Test;

# Regression pin for the multi-tier overlay env (docs/vm-dual-store.md).
# A scoped call frame chains a fresh overlay over the *whole* caller env
# (itself possibly scoped) instead of flattening it per nested call. The chain
# must (a) read enclosing lexicals through every tier, (b) keep each frame's
# own writes isolated (merge overlay-only on return), and (c) flatten correctly
# at capture boundaries (closures, gather, threads). This pins the behavior the
# per-nested-call deep-copy elimination must not break.

plan 19;

# --- deep nested method dispatch reads through the chain ---
class Chain {
    has $.n;
    method a() { self.b() }
    method b() { self.c() }
    method c() { self.d() }
    method d() { $!n * 2 }
}
is Chain.new(n => 21).a(), 42, 'four-deep self.method() chain';

# --- deep nested free-function calls reading an enclosing lexical ---
my $base = 100;
sub f1() { f2() }
sub f2() { f3() }
sub f3() { $base + 1 }
is f1(), 101, 'three-deep sub chain reads outer lexical';

# --- each frame's local writes do not leak to the caller ---
sub outer() {
    my $x = 1;
    inner();
    $x;   # must still be 1
}
sub inner() { my $x = 999; $x }
is outer(), 1, 'callee local does not clobber caller same-named local';

# --- a callee modifying a captured outer lexical propagates up the chain ---
my $acc = 0;
sub bump-outer() { $acc = $acc + 10 }
sub via() { bump-outer() }
via();
via();
is $acc, 20, 'nested call propagates write to outer lexical';

# --- method calls a free sub that calls a method (mixed chain) ---
class Mix {
    has $.k;
    method go() { helper($.k) }
    method twice() { $.k * 2 }
}
sub helper($v) { $v + Mix.new(k => $v).twice() }   # v + 2v = 3v
is Mix.new(k => 5).go(), 15, 'method -> sub -> method chain';

# --- closure captures a method-local across a nested method dispatch ---
class Cap {
    method run() {
        my $secret = 7;
        my $c = { $secret };
        self.apply($c);
    }
    method apply($c) { $c() + 1 }
}
is Cap.new.run(), 8, 'closure captures method-local through nested dispatch';

# --- closure created deep in a chain still reads its definition scope later ---
sub make-deep() {
    my $z = 3;
    return inner-make($z);
}
sub inner-make($z) { return { $z * 11 } }
my $clo = make-deep();
is $clo(), 33, 'closure returned up a chain keeps its captured value';

# --- top-level scalar capture-then-mutate (lexical alias) ---
my $v = 1;
my $r = { $v };
$v = 2;
is $r(), 2, 'closure sees later mutation of captured top-level scalar';

# --- per-iteration loop capture inside a method ---
class Loops {
    method collect() {
        my @c;
        for 1..3 -> $i { @c.push({ $i }) }
        @c.map({.()}).join(',');
    }
}
is Loops.new.collect(), '1,2,3', 'per-iteration capture inside a method';

# --- gather inside nested call sees outer lexicals through the chain ---
my $g = 5;
sub gathered() { gather { for 1..3 -> $i { take $i + $g } } }
sub call-gather() { gathered() }
is call-gather().join(','), '6,7,8', 'gather captures outer lexical via chain';

# --- factory closures keep independent captured state ---
sub factory($init) {
    my $count = $init;
    return { $count++; $count };
}
my $f-a = factory(10);
my $f-b = factory(20);
is $f-a(), 11, 'factory A first call';
is $f-a(), 12, 'factory A state persists';
is $f-b(), 21, 'factory B independent of A';

# --- recursion across the chain (deep frame stack) ---
sub fact($n) { $n <= 1 ?? 1 !! $n * fact($n - 1) }
is fact(6), 720, 'recursion through nested overlay frames';

# --- mutual recursion ---
sub is-even($n) { $n == 0 ?? True !! is-odd($n - 1) }
sub is-odd($n)  { $n == 0 ?? False !! is-even($n - 1) }
is is-even(10), True, 'mutual recursion even';
is is-odd(7), True, 'mutual recursion odd';

# --- dynamic variable visible through nested frames ---
my $*DYN = 'outer';
sub read-dyn() { $*DYN }
sub mid() { read-dyn() }
is mid(), 'outer', 'dynamic var read through nested frames';

# --- repeated independent method calls (no overlay leak between calls) ---
class Indep { has $.s; method v() { my $t = $.s; $t + 1 } }
my $obj = Indep.new(s => 41);
is $obj.v(), 42, 'first method call';
is $obj.v(), 42, 'second method call independent';
