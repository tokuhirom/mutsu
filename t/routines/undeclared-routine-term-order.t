use v6;
use Test;

# A bareword call is explained by a sigilless term, constant, enum value or
# parameter only when it is declared BEFORE the call, in a block that encloses
# the call (rakudo resolves terms lexically, in source order). A routine is
# hoisted. Expected results are rakudo 2026.09's.

plan 13;

sub main-run(Str $code) {
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err;
    my $out = $p.out.slurp(:close);
    my $err = $p.err.slurp(:close);
    ($p.exitcode, $out, $err)
}

for 'say x; my \x = 1',
    '{ say x; my \x = 1 }',
    'say x; { my \x = 1 }',
    'say aa; enum E <aa bb>' -> $code {
    my ($rc, $out, $err) = main-run($code);
    ok $rc != 0 && $err ~~ /'Undeclared routine'/, "refused: $code";
}

my @accepted =
    'my \x = 1; say x' => "1\n",
    'my \x = 1; { say x }' => "1\n",
    'constant k = 5; say k' => "5\n",
    'enum E <aa bb>; say aa' => "aa\n",
    'for 1,2 -> \w { say w }' => "1\n2\n",
    'sub f(\z) { z }; say f(3)' => "3\n",
    'foo; sub foo { say 1 }' => "1\n",
    '-> \y { say y }(2)' => "2\n";
for @accepted -> $case {
    my ($rc, $out, $err) = main-run($case.key);
    is $out, $case.value, "accepted: {$case.key}";
}

is EVAL('my \\q = 4; q + 1'), 5, 'EVAL: a term declared first explains the call';
