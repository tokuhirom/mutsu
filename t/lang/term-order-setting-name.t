use v6;
use Test;

# Rakudo hoists routines only: a constant, enum value or sigilless variable
# whose name the setting also has is the setting's until its declaration.

plan 11;

sub run-code(Str $code) {
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err;
    my $out = $p.out.slurp(:close);
    $p.err.slurp(:close);
    ($p.exitcode, $out.trim)
}

sub refused(Str $code) {
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err;
    my $out = $p.out.slurp(:close);
    my $err = $p.err.slurp(:close);
    $p.exitcode != 0 && $out eq '' && $err ~~ /'Undeclared routine'/
}

is-deeply run-code('say X; constant X = 3; say X'), (0, "(X)\n3"), 'constant';
is-deeply run-code('say Int; constant Int = 5; say Int'), (0, "(Int)\n5"), 'constant named like a core type';
is-deeply run-code('say X; my \X = 5; say X'), (0, "(X)\n5"), 'sigilless variable';
is-deeply run-code('sub f { X }; constant X = 4; say f; say X'), (0, "(X)\n4"), 'a routine body written before the declaration';
is-deeply run-code('{ say X; }; constant X = 3; say X'), (0, "(X)\n3"), 'a nested block written before the declaration';
is-deeply run-code('constant X = 3; say X'), (0, "3"), 'declared first';
is-deeply run-code('constant X = 3; { say X }'), (0, "3"), 'declared first, used in a block';
is-deeply run-code('{ constant X = 3; say X }; say X'), (0, "3\n(X)"), 'a block-local constant leaves scope';

# A name the setting does not have is as undeclared as a lowercase one: refused
# at compile time, before anything runs.
ok refused('say "ran"; say FOO; constant FOO = 3'), 'no setting name: constant';
ok refused('say "ran"; say Y; enum E <Y Z>'), 'no setting name: enum value';
ok refused('say "ran"; say Foo; my \Foo = 1'), 'no setting name: sigilless variable';
