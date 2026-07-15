use v6;
use Test;

# Parameterized grammar rules (99problems-41-to-50.t P47): `multi rule`
# candidates accumulate, dispatch prefers an exact literal-arg candidate
# (exclusively) over a generic ($p) one, recursion toward a base case
# (`<expr($p-1)>` at the same position) is not left recursion, and an
# arg-call inside a lookahead is instantiated with the bound params.

plan 8;

{
    grammar G1 {
        rule TOP { <expr(0)> }
        multi rule expr(0) { x }
        multi rule expr($p) { y }
    }
    ok G1.parse("x"), 'literal candidate matches its exact arg';
    nok G1.parse("y"), 'generic candidate is excluded when a literal matches';
}

{
    grammar G2 {
        rule TOP { <expr(2)> }
        multi rule expr(0) { \d+ }
        multi rule expr($p) { <expr($p-1)> }
    }
    ok G2.parse("42"), 'recursive descent through $p-1 args reaches the base case';
}

{
    grammar G3 {
        rule TOP { <expr(1)> }
        multi rule expr(0)  { \w }
        multi rule expr($p) { <expr($p-1)> *%[ <op> ] }
        token op { and }
    }
    ok G3.parse("A and B"), 'separated quantifier over a recursive arg-call';
    ok G3.parse("A"), 'zero separators still match';
}

{
    # The P47 precedence-climbing shape: lookahead on a parameterized token.
    grammar G4 {
        rule TOP { <expr(2)> }
        multi rule expr(0)   { <id> }
        multi rule expr($p)  { <expr($p-1)> *%[ <?before <pred($p)>> <op> ] }
        multi token pred(2) {or}
        multi token pred(1) {and}
        token id {<[ A .. Z ]>}
        token op {and|or}
    }
    ok G4.parse('A and B'), 'lookahead on <pred($p)> instantiates the bound param';
    ok G4.parse('A or B'), 'level-2 separator matches through level-1 recursion';
    nok G4.parse('A xor B'), 'an operator outside every pred level fails';
}
