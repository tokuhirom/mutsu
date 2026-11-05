use Test;
plan 5;

# `my $x will leave { ... }` at the top of a program: the trait is an exit
# phaser of the unit, run when the program ends, last declared first and mixed
# with the program's own `LEAVE { }` blocks in the same order. It was dropped
# (the declaration's scopeless block hid the phaser from the unit's phaser
# split), though inside a block or a sub it already ran.

sub run-program(Str $code --> Str) {
    my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
    $proc.out.slurp(:close) ~ $proc.err.slurp(:close)
}

is run-program('my $a will leave { say "a" }; my $b will leave { say "b" }; say "main"'),
    "main\nb\na\n", 'two will-leave traits run at the end, last declared first';

is run-program('my $a will leave { say "a" } = 2; say "main"'),
    "main\na\n", 'with an initializer';

# rakudo runs every `LEAVE { }` block first (last declared first), then the
# variables' traits (last declared first) -- not one interleaved reverse order.
is run-program('LEAVE { say "L1" }; my $a will leave { say "a" }; LEAVE { say "L2" }; my $b will leave { say "b" }; say "main"'),
    "main\nL2\nL1\nb\na\n", 'LEAVE blocks first, then the variable traits';

is run-program('our $o will leave { say "our" }; state $s will leave { say "state" }; say "main"'),
    "main\nstate\nour\n", 'our and state variables';

is run-program('{ my $a will leave { say "inner" }; say "body" }; say "main"'),
    "body\ninner\nmain\n", 'inside a block it still runs at the block end';
