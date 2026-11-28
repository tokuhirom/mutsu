# A parameterized subrule call whose arguments are all bare scalar variables
# or literal constants reads them without the evaluator (#12917). These pin
# that the callee binds exactly what an evaluated argument list gives it, and
# that every other argument shape still goes through the evaluator. Values are
# rakudo's.
use Test;

plan 13;

grammar Indent {
    token TOP { <line('')> }
    token line(Str $indent) { $indent 'x' [ \n <line($indent ~ ' ')> ]? }
}
ok Indent.parse("x\n x\n  x"), 'an expression argument grows the indent';

grammar Y {
    token TOP { <block('', 0)> }
    token block(Str $indent, Int $min) {
        :my $new;
        <?before $indent $<sp>=[' ' ** { $min..* }] { $new = $indent ~ $<sp> }>
        <entry($new, 1)>+ % \n
    }
    token entry(Str $indent, Int $depth) { $indent <[a..z]>+ { make $depth } }
}
my $y = Y.parse("  ab\n  cd");
ok $y, 'variables and literals as arguments';
is $y<block><entry>.elems, 2, 'every entry';
is $y<block><entry>[1].made, 1, 'the literal reaches the callee';

grammar Kebab {
    token TOP { :my $the-sep = ','; <list($the-sep)> }
    token list(Str $sep) { \d+ % $sep }
}
ok Kebab.parse('1,2,3'), 'a kebab-case :my variable as an argument';
nok Kebab.parse('1;2'), 'and it is the value the callee matches';

grammar Lit {
    token TOP { <rep('ab', 2)> }
    token rep(Str $s, Int $n) { [ $s ] ** { $n } }
}
ok Lit.parse('abab'), 'string and integer literals';
nok Lit.parse('ab'), 'the count is the literal';

grammar RatArg {
    token TOP { <r(1.5)> }
    token r($x) { \d+ { make $x * 2 } }
}
is RatArg.parse('7')<r>.made, 3, 'a Rat literal';

grammar Named {
    token TOP { <w(:sep('-'))> }
    token w(:$sep) { \w+ $sep \w+ }
}
ok Named.parse('a-b'), 'a named argument still binds';

grammar Pairs {
    token TOP { <p((a => 1))> }
    token p($pair) { \w+ { make $pair.key ~ $pair.value } }
}
is Pairs.parse('z')<p>.made, 'a1', 'a positional Pair argument';

{
    my $outer = 'q';
    grammar Outer {
        token TOP { <c($outer)> }
        token c(Str $ch) { $ch+ }
    }
    ok Outer.parse('qqq'), 'a variable from the enclosing scope';
    $outer = 'r';
    ok Outer.parse('rr'), 'read again at the next parse';
}
