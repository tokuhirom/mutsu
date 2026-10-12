use Test;

# A look-behind whose body calls a rule is bounded by that rule's body (the
# engine searches back only as far as the body can reach, #12917). The answers
# must be exactly those of a search from the start of the string.

plan 12;

grammar Sp {
    token TOP { [ <-[#]> | <!after <.sp>> '#' ]* }
    token sp { <[\ \t]> }
}
is Sp.parse('a#b').Str, 'a#b', 'a `#` not after a space matches';
nok Sp.parse('a #b'), 'a `#` after a space does not';

grammar Two {
    token TOP { \w+ <?after <.pair>> }
    token pair { \w ** 2 }
}
ok Two.parse('abc'), 'a two-character rule body is found behind the cursor';

grammar Proto {
    token TOP { .+ <?after <tail>> }
    proto token tail {*}
    token tail:sym<x> { <[x]> }
    token tail:sym<yz> { \w ** 2 }
}
ok Proto.parse('abyz'), 'the longest proto candidate bounds the search';
ok Proto.parse('abx'), 'a shorter candidate still matches';
nok Proto.parse('a!'), 'no candidate ends here';

grammar Nested {
    token TOP { .+ <?after <outer>> }
    token outer { <inner> }
    token inner { <[c]> }
}
ok Nested.parse('zabc'), 'a rule calling a rule is bounded through both';
nok Nested.parse('zab'), 'and fails where the inner rule does';

grammar Unbounded {
    token TOP { .+ <?after <word>> }
    token word { <[a..z]>+ }
}
ok Unbounded.parse('1abc'), 'an unbounded rule body is searched from the start';
nok Unbounded.parse('ab1'), 'and fails where it cannot end here';

grammar Marks {
    token TOP { .+ <?after <one>> }
    token one { . }
}
ok Marks.parse("ae\x[301]\x[302]"), 'a rule matching one grapheme with combining marks';

grammar Method {
    token TOP { .+ <?after <m>> }
    method m { self.new(:from(self.pos), :to(self.pos)) }
}
lives-ok { Method.parse('abc') }, 'a grammar method in a look-behind is not bounded';
