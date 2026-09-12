use Test;

# A constant subrule argument (`<rule($indent, 0)>`'s `0`) is its own value:
# nothing in it reads the env, the captures or the package. mutsu returns it
# straight from the parsed literal instead of building an evaluation env and a
# scratch interpreter, so this pins that every literal shape still arrives at
# the rule with the value it was written with.
#
# Verified against rakudo 2026.07.
# https://github.com/tokuhirom/mutsu/issues/7576

plan 6;

grammar Reps {
    token TOP { <run(3)> }
    token run(Int $n) { 'x' ** { $n } }
}
ok Reps.parse('xxx'), 'an integer literal argument reaches the rule';
nok Reps.parse('xx'), 'and is not off by one';

grammar Lit {
    token TOP { <word("hi")> <word('there')> }
    token word(Str $w) { \s* $w }
}
ok Lit.parse('hi there'), 'string literal arguments, both quote forms';

grammar Neg {
    token TOP { <shifted(-1)> }
    token shifted(Int $n) { 'y' ** { $n + 3 } }
}
ok Neg.parse('yy'), 'a negative integer literal keeps its sign';

grammar Rat {
    token TOP { <scaled(0.5)> }
    token scaled($f) { 'z' ** { ($f * 4).Int } }
}
ok Rat.parse('zz'), 'a rational literal keeps its value';

grammar Bool {
    token TOP { <maybe(True)> }
    token maybe($on) { 'w' ** { $on ?? 2 !! 5 } }
}
ok Bool.parse('ww'), 'a boolean literal keeps its truth';
