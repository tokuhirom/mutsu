use v6;
use Test;

# Rakudo hoists routines only. A sigilless variable, a constant and an enum key
# are terms from their declaration on, so a bareword naming one before its
# declaration is `Undeclared routine`, in the same statement list or a nested
# one; used after it (in the list or below it) it is the term.

plan 20;

sub run-code(Str $code) {
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err;
    my $out = $p.out.slurp(:close);
    my $err = $p.err.slurp(:close);
    ($p.exitcode, $out, $err)
}

for 'say x; my \x = 1',
    '{ say x; my \x = 1 }',
    'say x; { my \x = 1 }',
    'say x; constant x = 1',
    'say x; my constant x = 1',
    'say x; enum E <x y>',
    'sub f { x }; my \x = 1; f',
    'class C { method m { x } }; my \x = 1' -> $code {
    my ($rc, $out, $err) = run-code($code);
    ok $rc != 0 && $err ~~ /'Undeclared routine'/ && $out eq '', "refused: $code";
}

for 'my \x = 1; say x',
    'my \x = 1; { say x }',
    'my \x = 1; sub f { x }; say f',
    'constant x = 2; say x',
    'enum E <x y>; say x',
    'say x; sub x { 7 }',
    'sub x { 7 }; { my \y = 2; say x() + y }',
    'class C { constant k = 3; method m { k } }; say C.m' -> $code {
    my ($rc, $out, $err) = run-code($code);
    ok $rc == 0 && $out.chars > 0, "accepted: $code";
}

# A term leaves scope with its block.
{
    my ($rc, $out, $err) = run-code('{ my \x = 1; say x }; say x');
    ok $rc != 0 && $err ~~ /'Undeclared routine'/, 'a block-local term is gone after the block';
}

# A term is not a variable of the same spelling.
{
    my ($rc, $out, $err) = run-code('my \x = 1; END { say x }');
    ok $rc == 0 && $out.trim eq '1', 'a phaser body sees the term declared before it';
}

# The error is the CHECK-time one: nothing ran before it.
{
    my ($rc, $out, $err) = run-code('say "ran"; say x; my \x = 1');
    ok $out eq '' && $err ~~ /'x used'/, 'reported before anything runs';
}

# EVAL judges its own unit the same way.
throws-like q[EVAL 'say x; my \x = 1'], X::Undeclared::Symbols, 'EVAL: use before the term';
