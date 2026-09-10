use Test;

plan 14;

# A Unicode infix alias names the same routine as its ASCII spelling, so
# reaching it by name -- `&infix:<X>`, or the runtime string form that
# Test.rakumod's `cmp-ok` uses -- has to resolve to the same operator.
ok  &infix:<⩵>(4, 4),      '&infix:<⩵> is ==';
ok  &infix:<⩶>(4, 4),      '&infix:<⩶> is ===';
nok &infix:<≠>(4, 4),      '&infix:<≠> is !=';
ok  &infix:<≤>(4, 4),      '&infix:<≤> is <=';
ok  &infix:<≥>(4, 4),      '&infix:<≥> is >=';
ok  &infix:<≅>(4, 4),      '&infix:<≅> is =~=';
is  &infix:<−>(9, 4), 5,   '&infix:<−> is -';
is  &infix:<×>(9, 4), 36,  '&infix:<×> is *';
is  &infix:<÷>(9, 3), 3,   '&infix:<÷> is /';

# The same names built at runtime, which is the shape `cmp-ok` reaches them by.
my $op = '≅';
ok &CALLER::LEXICAL::("infix:<$op>")(4, 4), 'the runtime string form resolves too';
$op = '≤';
ok &CALLER::LEXICAL::("infix:<$op>")(4, 9), 'and so does another alias';

cmp-ok 4, '≅', 4,  'cmp-ok takes the alias as a string operator';
cmp-ok 4, '≤', 9,  'cmp-ok takes ≤ too';
cmp-ok 4, '≠', 9,  'cmp-ok takes ≠ too';
