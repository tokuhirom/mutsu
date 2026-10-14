use Test;

# An imaginary literal whose part is a Rat with a denominator beyond 64 bits
# (`1e-20i`, rendered as `0.00000000000000000001i`) must lower back to the
# Complex it denotes, not to Nil.

plan 3;

is Q|1e-20i|.AST.EVAL.raku, '<0+1e-20i>', 'a tiny imaginary literal survives EVAL';
is Q|3 + 1e-20i|.AST.EVAL.raku, '<3+1e-20i>', 'and inside an addition';
is Q|0.5i|.AST.EVAL.raku, '<0+0.5i>', 'a small-denominator Rat part still works';
