use Test;

# Closures only persist/restore their *free* variables (the ones their body
# actually references). These tests pin the observable behavior of that
# per-instance captured-state machinery so the free-variable optimization in
# call_compiled_closure cannot regress it.

plan 10;

# Two closures from the same factory keep independent mutable captured state.
sub make-counter() {
    my $n = 0;
    return -> { $n++; $n };
}
my $a = make-counter();
my $b = make-counter();
is $a(), 1, 'first counter starts at 1';
is $a(), 2, 'first counter increments';
is $b(), 1, 'second counter is independent';
is $a(), 3, 'first counter unaffected by second';
is $b(), 2, 'second counter increments independently';

# A `state` variable inside a closure persists per-instance across calls.
my $c = -> { state $s = 0; $s = $s + 10; $s };
is $c(), 10, 'state var first call';
is $c(), 20, 'state var persists across calls';
is $c(), 30, 'state var keeps accumulating';

# A closure mutating a captured variable is visible to the enclosing scope.
sub outer() {
    my $x = 100;
    my $bump = -> { $x = $x + 5 };
    $bump();
    $bump();
    $x;
}
is outer(), 110, 'captured variable mutation visible to enclosing scope';

# A nested closure capturing a grandparent lexical mutates it through both levels.
sub grand() {
    my $g = 1;
    my $mid = -> {
        my $inner = -> { $g = $g * 2; $g };
        $inner(); $inner();
    };
    $mid();
    $g;
}
is grand(), 4, 'nested closure mutates grandparent lexical';
