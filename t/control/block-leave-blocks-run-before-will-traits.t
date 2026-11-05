use Test;
plan 10;

# In a block or a sub, every `LEAVE { }` block runs first (last declared
# first), then the variables' `will leave` traits (last declared first) --
# rakudo registers a variable's exit traits apart from the blocks, so the two
# do not interleave in one reverse declaration order.

sub run-program(Str $code --> Str) {
    my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
    $proc.err.slurp(:close);   # the order is what is checked, not compile warnings
    $proc.out.slurp(:close)
}

my $block = 'LEAVE { say "L1" }; my $a will leave { say "a" }; LEAVE { say "L2" }; '
          ~ 'my $b will leave { say "b" }; LEAVE { say "L3" }; say "body"';

is run-program('{ ' ~ $block ~ ' }'), "body\nL3\nL2\nL1\nb\na\n",
    'a bare block: the LEAVE blocks, then the traits';
is run-program('sub f { ' ~ $block ~ ' }; f()'), "body\nL3\nL2\nL1\nb\na\n",
    'a sub body';
is run-program('for 1 { ' ~ $block ~ ' }'), "body\nL3\nL2\nL1\nb\na\n",
    'a loop body';

# Traits alone keep their reverse declaration order.
is run-program('{ my $a will leave { say "a" }; my $b will leave { say "b" }; say "body" }'),
    "body\nb\na\n", 'two traits, last declared first';

# A trait with an initializer.
is run-program('{ LEAVE { say "L" }; my $a will leave { say "a" } = 5; say "body" }'),
    "body\nL\na\n", 'a trait on an initialized variable runs after the LEAVE block';

# No LEAVE block at all: nothing changes.
is run-program('{ my $a will leave { say "a" }; say "body" }'),
    "body\na\n", 'a lone trait';

# A loop body's own exit phasers also run last declared first (they ran in
# declaration order on the normal path, though the early-exit path was right).
is run-program('for 1, 2 { LEAVE { say "L1" }; LEAVE { say "L2" }; say "body" }'),
    "body\nL2\nL1\nbody\nL2\nL1\n", 'two LEAVE blocks in a loop body, every iteration';

is run-program('for 1 { KEEP { say "K1" }; KEEP { say "K2" }; say "body"; "ok" }'),
    "body\nK2\nK1\n", 'two KEEP blocks in a loop body';

is run-program('for 1 { UNDO { say "U1" }; UNDO { say "U2" }; say "body"; Nil }'),
    "body\nU2\nU1\n", 'two UNDO blocks in a loop body';

is run-program('loop (my $i = 0; $i < 1; $i++) { LEAVE { say "L1" }; LEAVE { say "L2" }; say "body" }'),
    "body\nL2\nL1\n", 'a C-style loop body';
