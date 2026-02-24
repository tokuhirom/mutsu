use Test;

plan 4;

is roundrobin(1).join, '1', 'roundrobin scalar identity';
is roundrobin($(1, 2), <a b c>), (($(1, 2), 'a'), ('b',), ('c',)),
    'roundrobin keeps itemized arguments as single items';
is roundrobin(<a b c>, $(1, 2)), (('a', $(1, 2)), ('b',), ('c',)),
    'roundrobin keeps itemized arguments in non-leading position';

my %h = %(:a);
is-deeply roundrobin(%h<>:v.map: *.flat), ((True,),),
    'roundrobin handles zen-angle values map with whatever flat';
