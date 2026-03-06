use Test;

plan 6;

is ([+] 1, 2, 3 X** 2, 4), 112, 'reduction keeps X-list precedence';

my Mu $orelse-topic is default(Nil) = 0;
Nil Xorelse ($orelse-topic = $_,);
ok $orelse-topic === Nil, 'Xorelse topicalizes Nil correctly';

my @foo = 0 xx 3;
@foo X= 1;
is @foo, '1 1 1', 'X= assignment updates each left element';

my @lhs = 1, 2, 3;
@lhs X*= 10;
is @lhs, '10 20 30', 'X*= assignment works';

is-deeply &infix:<X+>((1, 2, 3), (4, 5, 6)), (5, 6, 7, 6, 7, 8, 7, 8, 9),
    '&infix:<X+> autogenerates';

my %h = %(:a);
is-deeply cross(%h<>:v.map: *.flat), ((True,),),
    'cross keeps one-element list shape';
