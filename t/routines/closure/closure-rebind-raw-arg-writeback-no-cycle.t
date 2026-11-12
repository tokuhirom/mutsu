use Test;

# A variable a closure rebinds with `:=` holds a binding cell. Passing it to an
# `is raw` / `is rw` parameter binds the parameter to that very cell, and the
# call-return writeback then stored the binding's content -- a reference to the
# variable's own container -- into that container: the cell contained itself.
# Every later read, and every `start` (which walks the spawning frame's
# variables), then looped forever. Found running ParaSeq, whose batches pass
# such variables to `is raw` parameters (every degree > 1 `.sum`, `.map`, ...
# hung).

plan 6;

sub take-raw($x is raw) { 1 }
sub take-rw($x is rw) { $x = $x + 10 }

sub raw-then-spawn() {
    my $p = 1;
    my &rebind = { $p := 2 };
    take-raw($p);
    my $copy = $p;
    await start { $p + $copy };
}
is raw-then-spawn(), 2, 'an is-raw call on a rebindable variable leaves it readable from a thread';

sub rw-writes-through() {
    my $p = 1;
    my &rebind = { $p := 5 };
    take-rw($p);
    $p
}
is rw-writes-through(), 11, 'an is-rw call still writes the value back';

sub rebind-after-call() {
    my $p = 1;
    my &rebind = { $p := 7 };
    take-raw($p);
    rebind();
    $p
}
is rebind-after-call(), 7, 'the closure rebind still reaches the declaring frame';

sub hash-raw() {
    my %h = a => 1;
    my &rebind = { %h := {b => 2} };
    take-raw(%h);
    await start { %h<a> };
}
is hash-raw(), 1, 'a rebindable hash passed is-raw stays readable from a thread';

sub two-calls() {
    my $p = 3;
    my &rebind = { $p := 4 };
    take-raw($p);
    take-raw($p);
    await start { $p };
}
is two-calls(), 3, 'repeated is-raw calls do not chain the container';

sub rw-then-spawn() {
    my $p = 1;
    my &rebind = { $p := 2 };
    take-rw($p);
    await start { $p };
}
is rw-then-spawn(), 11, 'an is-rw call on a rebindable variable, then a thread';
