use v6;
use Test;

# Regression (#7954, the `expected statement ...` parse-failure index): a
# user-declared infix operator continues an expression onto the next line, just
# as a built-in symbolic one already did. mutsu refused any word-shaped custom
# infix after a newline, because the word matcher is deliberately speculative —
# it accepts ANY bareword so a runtime-installed operator still parses — and a
# speculative match across a newline would swallow the next statement. The
# guard was applied to the DECLARED case too, so a statement wrapped after its
# first operand died with "Undeclared routine" (Arithmetic::PaperAndPencil
# wraps a long `☈+` expression exactly that way).

plan 10;

sub infix:<qq7>($a, $b) { $a + $b }
sub infix:<☈+>($a, $b) { $a * 10 + $b }

# The construct the index reduced to: the operator opens the continuation line.
my $wrapped = 1
    qq7 2;
is $wrapped, 3, 'a declared word infix continues an assignment across a newline';

is (1
    qq7 2), 3, 'and continues a parenthesized expression';

sub returns-wrapped() {
    return 1
        ☈+ 2;
}
is returns-wrapped(), 12, 'a declared symbolic infix continues a return expression';

# Several continuation lines in a row fold left, exactly as on one line.
my $chained = 1
    qq7 2
    qq7 3;
is $chained, 6, 'successive continuation lines fold left';
is (1 qq7 2 qq7 3), 6, 'and agree with the same expression on one line';

# A built-in symbolic infix on a continuation line was always accepted; it
# still is.
my $builtin = 1
    + 2;
is $builtin, 3, 'a built-in infix still continues across a newline';

# The speculative case must stay off: an UNDECLARED bareword opening the next
# line is a new statement, not an operator.
my $first = 1;
say-nothing();
is $first, 1, 'an undeclared bareword on the next line stays a new statement';
sub say-nothing() { }

# A word that merely starts with an operator's name is not the operator.
sub qq7ish($x) { $x + 100 }
is (qq7ish 1), 101, 'a longer identifier is not the infix';

# And the operator still works in its ordinary same-line position.
is (4 qq7 5), 9, 'the word infix works on one line';
is (4 ☈+ 5), 45, 'the symbolic infix works on one line';
