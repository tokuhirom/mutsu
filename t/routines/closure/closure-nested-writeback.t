use Test;

# Regression pin for the closure exit-writeback skip optimization.
#
# A closure that makes a call is NOT a leaf: a nested method/closure invoked in
# its body may close over an enclosing lexical and write it back into this
# frame's env, even when that lexical is not a free variable of the outer
# closure itself. Such mutations must still propagate to the caller. An earlier
# version of the "skip the writeback scan for read-only closures" optimization
# dropped them (regressing roast/integration/advent2011-day03.t et al.).

plan 4;

# Minimal reproduction of the advent2011 `capture-out` pattern: $*OUT is a
# class whose methods close over $output; the passed-in closure calls
# $*OUT.print, which mutates $output even though the closure never names it.
sub capture-out($code) {
    my $output = "";
    my $*OUT = class {
        method print(*@args) { $output ~= @args.join }
    }
    $code();
    return $output;
}
is capture-out({ $*OUT.print("hello") }), "hello",
    "nested method call mutates enclosing lexical via dynamic var";
is capture-out({ $*OUT.print("a"); $*OUT.print("b"); $*OUT.print("c") }), "abc",
    "repeated nested mutations all propagate";

# A closure that calls a helper sub which mutates a captured outer variable
# through a second closure stored in the same scope.
sub accumulate(@items) {
    my $acc = 0;
    my $add = -> $n { $acc = $acc + $n };
    my $apply = -> $blk, $v { $blk($v) };
    for @items -> $x {
        # $apply is not a leaf (it calls $blk); $acc is not its free var.
        $apply($add, $x);
    }
    return $acc;
}
is accumulate([1, 2, 3, 4]), 10,
    "non-leaf closure forwarding a call that mutates an outer lexical";

# Sanity: a genuinely leaf read-only block still works (the fast path).
sub transform(@items) {
    my $factor = 10;
    return @items.map({ $_ * $factor }).sum;
}
is transform([1, 2, 3]), 60, "leaf read-only map block still computes correctly";
