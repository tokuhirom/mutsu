use v6;
use Test;

# A type used before the statement that declares it is rakudo's
# `Illegally post-declared type`, in every kind of unit: the main program, a
# module, an EVAL string; and also inside a nested block or a routine body.
# Declared-first, stubbed and core-named types are fine.

plan 14;

sub main-run(Str $code) {
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err;
    my $err = $p.err.slurp(:close);
    $p.out.slurp(:close);
    ($p.exitcode, $err)
}

for 'say A; class A {}',
    '{ say B; class B {} }',
    'sub f { A.new }; class A {}',
    'class A { method m { B.new } }; class B {}',
    'my $x = B; class B {}' -> $code {
    my ($rc, $err) = main-run($code);
    ok $rc != 0 && $err ~~ /'post-declared'/, "main program: $code";
}

{
    my $p = run $*EXECUTABLE, '-I', 't/fixtures/post-declared-lib', '-e',
        'use PostDeclaredBad; say 1', :out, :err;
    my $err = $p.err.slurp(:close);
    ok $p.exitcode != 0 && $err ~~ /'post-declared'/, 'a module that post-declares a type is refused';
}
{
    my $p = run $*EXECUTABLE, '-I', 't/fixtures/post-declared-lib', '-e',
        'use PostDeclaredGood; say make().^name', :out, :err;
    ok $p.out.slurp(:close).trim.ends-with('Early'), 'a module that declares first loads';
    $p.err.slurp(:close);
}

throws-like q[say EvalOne; class EvalOne {}], X::Undeclared::Symbols, 'EVAL: a use before the class';
throws-like q[sub f { EvalTwo.new }; class EvalTwo {}], X::Undeclared::Symbols, 'EVAL: inside a routine body';

for 'class A {}; say A',
    'role R {...}; say R; role R { }',
    'class A { method m { A.new } }',
    'say Set; class Set {}',
    'class Ns::Y::Z { }; my \x = Ns::Y; class Ns::Y { }' -> $code {
    my ($rc, $err) = main-run($code);
    ok $rc == 0, "accepted: $code";
}
