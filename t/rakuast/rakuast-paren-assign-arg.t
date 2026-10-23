use Test;

# A parenthesised assignment written as a call argument keeps its
# `Circumfix::Parentheses` in the AST, exactly like `($s = 5)` as a statement.
# Checked against `raku` first; this file must pass under both.

plan 7;

sub count-parens(Str $src) { $src.AST.gist.comb('Circumfix::Parentheses').elems }

is count-parens(Q{sub foo(*@a) {}; my $s; foo(1, ($s = 5))}), 1, 'second argument';
is count-parens(Q{sub foo(*@a) {}; my $s; foo(($s = 5))}), 1, 'sole argument';
is count-parens(Q{my $s; ($s = 5)}), 1, 'statement form (unchanged)';
is count-parens(Q{sub foo(*@a) {}; my $s; foo($s = 5)}), 0, 'no parentheses written, none rendered';

sub foo(*@a) { @a.elems }
my $s;
is foo(1, ($s = 5)), 2, 'a parenthesised assignment is one argument';
is $s, 5, 'and it assigns';
is EVAL(Q{sub bar(*@a) { @a.join(",") }; my $t; bar(1, ($t = 5))}.AST), '1,5', 'round trip evaluates';
