use v6;
use Test;

# Two calls to the SAME subrule at the same position are siblings, not left
# recursion. The streamed subrule path held its left-recursion activation across
# the continuation, so the second call read that activation's empty seed and
# failed.
#
# Every `rule` with a bracketed group compiles to exactly that shape — sigspace
# puts a `<.ws>` at the end of the group AND right after it
# (`'[' <.ws> [ <id> <.ws> ] <.ws> ']'`) — so a grammar that defines its own
# `ws` stopped matching any such rule. CSS::Grammar does
# (`token ws { <!ww>[ <.wc> | <.comment> ]* }`), which took out `attrib`,
# `pseudo-function` and most of the CSS3 selector grammar.

plan 8;

grammar A {
    token zz { \s* }
    token id { <[\w-]>+ }
    token flat     {   <id> <.zz>   <.zz> ']' }
    token grouped  { [ <id> <.zz> ] <.zz> ']' }
    token grouped3 { [ <id> <.zz> ] <.zz> <.zz> ']' }
    token inner    { [ <id> <.zz> ] ']' }
    token outer    { [ <id> ]       <.zz> ']' }
    token consumes { [ <id> <.zz> ] <.zz> ']' }
}

ok A.subparse('hello]', :rule<flat>).defined,     'two flat sibling calls at one position';
ok A.subparse('hello]', :rule<grouped>).defined,  'a group-final call and the call after it';
ok A.subparse('hello]', :rule<grouped3>).defined, 'three sibling calls at one position';
ok A.subparse('hello]', :rule<inner>).defined,    'the group-final call alone';
ok A.subparse('hello]', :rule<outer>).defined,    'the call after the group alone';
ok A.subparse('hello  ]', :rule<consumes>).defined, 'the inner call consuming moves the outer one';

grammar CustomWs {
    token wc { \n | "\t" | " " }
    token ws { <!ww>[ <.wc> ]* }
    token id { <[\w-]>+ }
    rule  bracketed { '[' [ <id> ] ']' }
}
ok CustomWs.subparse('[hello]', :rule<bracketed>).defined,
    'a bracketed group in a rule of a grammar with its own `ws`';
ok CustomWs.subparse('[ hello ]', :rule<bracketed>).defined,
    '... and with real whitespace to consume';
