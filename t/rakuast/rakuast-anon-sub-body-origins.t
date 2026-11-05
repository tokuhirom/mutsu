use Test;

# The body of an anonymous `sub` / `method` has the ranges rakudo 2026.09
# gives it (#16643): the `Blockoid` from its `{` to the end of the routine,
# and its `StatementList` from the first statement to the end of the last
# (the `;` terminators included), or right after the `{` when empty.

plan 14;

use experimental :rakuast;

sub rng($n) { my $o = $n.origin; $o ?? "{$o.from}-{$o.to}" !! "none" }
sub body(Str $src) {
    my $b = $src.AST.statements[0].expression.body;
    rng($b) ~ " " ~ rng($b.statement-list)
}

is body('sub ($a) { 1 }'), "9-14 11-12", 'one statement';
is body('sub () { 1 }'), "7-12 9-10", 'an empty signature';
is body('sub ($a) returns Int { 1 }'), "21-26 23-24", 'a return type before the body';
is body('method ($x) { 2 }'), "12-17 14-15", 'a method literal';
is body('sub ($a) is raw { 1 }'), "16-21 18-19", 'a trait before the body';
is body('sub ($a) {  }'), "9-13 10-10", 'an empty body';
is body('sub ($a) { 1; }'), "9-15 11-13", 'the terminator is in the list';
is body('sub ($a) { 1; 2 }'), "9-17 11-15", 'two statements';
is body('sub ($a) { my $x = 1; $x }'), "9-26 11-24", 'a declaration and a use';
is body("sub (\$a) \{\n  say 1;\n  say 2;\n}"), "9-30 13-28", 'statements on their own lines';
is body('sub ($a) { { 3 } }'), "9-18 11-16", 'a nested block';
is body('sub (|c) { c }'), "9-14 11-12", 'a capture parameter';
is body('sub ($a, $b) { $a + $b }'), "13-24 15-22", 'two parameters';
is rng('sub ($a) { 1 }'.AST.statements[0].expression), "0-14", 'the routine itself (unchanged)';
