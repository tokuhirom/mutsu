use Test;

plan 4;

#| first
sub a { }
=begin pod
mid
=end pod
#| second
sub b { }
=begin pod
last
=end pod

is $=pod.elems, 4, 'four blocks in $=pod';
is $=pod.map(*.^name.split('::').tail).join(" "), 'Declarator Named Declarator Named',
    'declarator and Pod blocks are in source order';
is $=pod[0].WHEREFORE.^name, 'Sub', 'the first block documents sub a';
is $=pod[2].leading, 'second', 'the third block is the second declarator';
