use Test;

# #9617: inside an LTM declarative-prefix measurement, a `|` branch is ranked
# from the walk that collected its ends instead of being measured a second
# time. The ranking (furthest end or fate) must stay what the separate
# measurement gave. Expected values checked against `raku`.

plan 9;

# The #9617 shape as a `regex`: every loop step ranks `<A> | .`.
grammar G1 {
    token TOP { <A> }
    regex A { '{' [ <A> | . ]*? '}' }
}
ok G1.parse('{' ~ ('{ab{c}d}' x 20) ~ '}'), 'a recursive frugal regex parses';
is G1.parse('{ab{c}d}')<A><A>.map(*.Str).join(','), '{c}', 'and keeps its nested match';

# A fate (`<.ws>`) inside a measured subrule's `|` branch: the branch
# measures to the fate, so the literal outranks the subrule.
grammar G2 {
    token TOP { [ <B> | 'xy' ] .* }
    token B { [ 'x' <.ws> 'yz' | 'x' ] }
}
nok G2.subparse('xyz')<B>:exists, 'a fate inside a nested branch bounds its rank';

# The nested alternation's longer branch carries the subrule past the literal.
grammar G3 {
    token TOP { [ <C> | 'ab' ] }
    token C { [ 'a' | 'abc' ] 'd'? }
}
is ~G3.subparse('abcd'), 'abcd', 'a nested branch extends the measured prefix';
ok G3.subparse('abcd')<C>:exists, 'and the subrule wins';

# A code block is a fate: the nested branch measures only up to it.
grammar G4 {
    token TOP { [ <D> | 'aaa' ] }
    token D { 'a' [ 'a' { } 'a' 'a' | 'a' ] }
}
is ~G4.subparse('aaaa'), 'aaa', 'a code block ends the nested branch';
nok G4.subparse('aaaa')<D>:exists, 'so the literal wins';

# Two levels of nested subrules, each with its own `|`.
grammar G5 {
    regex TOP { [ <E> | 'pq' ] 'r' }
    regex E { 'p' [ <F> | 'q' ] }
    regex F { 'q' 'r' 's' | 'qq' }
}
is ~G5.parse('pqr'), 'pqr', 'nested subrule alternations rank through';
ok G5.parse('pqr')<E>:exists, 'with the subrule branch taken';
