use Test;

# Regression pins for the precise closure env-sync analysis: a frame may hold
# many locals while a closure created in it captures only a few. The compiler
# must flush exactly the captured (free) variables to env before the capture,
# no more and no less. Over-syncing was the old conservative behaviour; this
# test guards against under-syncing (stale captures) after the precision change.

plan 8;

# A closure captures one of several sibling locals and mutates it; the other
# siblings must be untouched and the mutation must be visible to the caller.
sub selective() {
    my $a = 1;
    my $b = 2;
    my $c = 3;
    my $d = 4;
    my $f = { $b = $b + 10 };
    $f();
    "$a $b $c $d";
}
is selective(), "1 12 3 4", "closure mutates only the captured sibling local";

# A read-only closure sees the latest value of a captured local even when the
# local is assigned after the closure is built.
sub deferred() {
    my $x = 1;
    my $g = { $x };
    $x = 99;
    $g();
}
is deferred(), 99, "read-only closure sees post-build assignment to captured local";

# Nested closure mutating a grandparent local through two capture levels.
sub nested() {
    my $n = 0;
    my $outer = {
        my $inner = { $n = $n + 1 };
        $inner();
        $inner();
    };
    $outer();
    $n;
}
is nested(), 2, "nested closure mutates grandparent local transitively";

# String interpolation of a captured local inside a closure.
sub interp() {
    my $name = "world";
    my $extra = "unused";
    my $h = { "hello $name" };
    $h();
}
is interp(), "hello world", "string interpolation captures outer local";

# map/grep blocks capturing an outer local that is not otherwise referenced.
sub mapgrep() {
    my $mul = 3;
    (1, 2, 3).map({ $_ * $mul }).grep({ $_ > 3 }).join(",");
}
is mapgrep(), "6,9", "map/grep blocks capture outer local";

# Each closure instance from a factory keeps independent captured state.
sub make-counter($start) {
    my $n = $start;
    -> { $n = $n + 1; $n };
}
my $c1 = make-counter(0);
my $c2 = make-counter(100);
my $a1 = $c1(); my $a2 = $c1(); my $a3 = $c1();
is "$a1,$a2,$a3", "1,2,3", "first counter has its own state";
my $b1 = $c2(); my $b2 = $c2();
is "$b1,$b2", "101,102", "second counter is independent";

# A closure that reads a captured local but never assigns it must not leak any
# of the parent frame's purely-local (non-captured) variables.
sub no-leak() {
    my $captured = 5;
    my $only-local = 999;
    my $r = { $captured * 2 };
    $r();
}
is no-leak(), 10, "read-only closure returns captured-derived value";
