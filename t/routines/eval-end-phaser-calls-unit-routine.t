use Test;

# An END an EVAL'd unit installs runs at program exit, after the unit ended,
# and still calls the routines the unit declared (they are lexical to it).
# Each program runs in a sub-process so the real exit-time order is observed.
# Checked against rakudo.

plan 5;

sub out-of(Str $code) {
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err;
    $p.out.slurp(:close).lines.join('|');
}

is out-of(Q[use MONKEY-SEE-NO-EVAL;
EVAL(q:to/CODE/);
sub foo($x) { say "foo $x" }
END { foo(1) }
CODE
say "after";]), 'after|foo 1', 'an END of a string EVAL calls the unit sub';

is out-of(Q[use MONKEY-SEE-NO-EVAL; use experimental :rakuast;
EVAL(q:to/CODE/.AST);
sub foo($x) { say "foo $x" }
END { foo(1) }
CODE
say "after";]), 'after|foo 1', 'and as EVAL($ast)';

is out-of(Q[use MONKEY-SEE-NO-EVAL;
EVAL(q:to/CODE/);
multi sub twice(Int $x) { say "int $x" }
multi sub twice(Str $x) { say "str $x" }
END { twice(2); twice("s") }
CODE
say "after";]), 'after|int 2|str s', 'a multi family of the unit';

is out-of(Q[use MONKEY-SEE-NO-EVAL;
EVAL('sub first-unit { say "first" }; END { first-unit() }');
EVAL('sub second-unit { say "second" }; END { second-unit() }');
say "after";]), 'after|second|first', 'ENDs of two units run in reverse order, each with its own routine';

is out-of(Q[use MONKEY-SEE-NO-EVAL;
EVAL('sub only-in-eval { 1 }; END { only-in-eval() }');
say (try EVAL(q[only-in-eval()])).defined ?? "leaked" !! "gone";]), 'gone', 'the unit routine does not leak into the program';
