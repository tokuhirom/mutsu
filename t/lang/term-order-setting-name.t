use v6;
use Test;

# Rakudo hoists routines only: a constant, enum value or sigilless variable
# whose name the setting also has is the setting's until its declaration.

plan 8;

sub run-code(Str $code) {
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err;
    my $out = $p.out.slurp(:close);
    $p.err.slurp(:close);
    ($p.exitcode, $out.trim)
}

is-deeply run-code('say X; constant X = 3; say X'), (0, "(X)\n3"), 'constant';
is-deeply run-code('say Int; constant Int = 5; say Int'), (0, "(Int)\n5"), 'constant named like a core type';
is-deeply run-code('say X; my \X = 5; say X'), (0, "(X)\n5"), 'sigilless variable';
is-deeply run-code('sub f { X }; constant X = 4; say f; say X'), (0, "(X)\n4"), 'a routine body written before the declaration';
is-deeply run-code('{ say X; }; constant X = 3; say X'), (0, "(X)\n3"), 'a nested block written before the declaration';
is-deeply run-code('constant X = 3; say X'), (0, "3"), 'declared first';
is-deeply run-code('constant X = 3; { say X }'), (0, "3"), 'declared first, used in a block';
is-deeply run-code('{ constant X = 3; say X }; say X'), (0, "3\n(X)"), 'a block-local constant leaves scope';
