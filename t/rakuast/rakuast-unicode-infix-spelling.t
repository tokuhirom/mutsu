use Test;
use experimental :rakuast;

# An infix written in one of its Unicode spellings keeps it in the tree, as
# measured on rakudo 2026.09, and still runs as the operator it aliases.
# This file passes under both mutsu and raku.

plan 22;

sub infix-of(Str $op) {
    "my \$a; my \$b; \$a $op \$b".AST.statements[2].expression.infix.raku
}

for <× ÷ − ≤ ≥ ≠ ⩵ ⩶ ≅ ∘ ∈ ∋ ⊆ ⊇ ⊂ ⊃> -> $op {
    is infix-of($op), qq[RakuAST::Infix.new("$op")], "$op keeps its spelling";
}
is infix-of('*'), Q[RakuAST::Infix.new("*")], 'an ASCII operator is unchanged';
is Q[1 … 3].AST.statements[0].expression.infix.raku, Q[RakuAST::Infix.new("…")],
    'a sequence keeps …';

is EVAL(Q[5 − 2].AST), 3, 'EVAL: − subtracts';
is EVAL(Q[1 ≤ 2].AST), True, 'EVAL: ≤ compares';
is EVAL(Q[1 … 4].AST).List, (1, 2, 3, 4), 'EVAL: … is a sequence';
is EVAL(Q[(* + 1) ∘ (* * 2)].AST)(3), 7, 'EVAL: ∘ composes';
