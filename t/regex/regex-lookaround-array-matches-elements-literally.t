use Test;

# A bare `@name` inside a lookaround matches any of the array's elements
# literally, as it does outside one; `<@name>` is the form that reads them as
# regexes. A dynamic `@*name` interpolates the same way (#13928).

plan 6;

my @f = '|';
is ~('ab|c' ~~ / [ <!before @f> . ]+ /), 'ab',
    'a lookahead over an array matches its elements literally';

my @g = '.', '+';
is ~('a+b.c' ~~ / [ <!before @g> . ]+ /), 'a',
    '... every element of it';

grammar Assigned {
    token TOP { :my @*forbid; { @*forbid = '>', '|' } <txt>+ }
    token txt { <!before @*forbid> . }
}
nok Assigned.parse('ab|c'), 'a lookahead over a dynamic array assigned in a block';
ok Assigned.parse('abc'), '... still matches what the array does not hold';

grammar Initialized {
    token TOP { :my @*forbid = '>', '|'; <txt>+ }
    token txt { <!before @*forbid> . }
}
nok Initialized.parse('ab|c'), 'a lookahead over a dynamic array with an initializer';

grammar Plain {
    token TOP { :my @*forbid = 'x'; <txt>+ }
    token txt { @*forbid }
}
ok Plain.parse('xx'), 'a dynamic array outside a lookaround matches its elements';

# vim: expandtab shiftwidth=4
