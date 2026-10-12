use Test;

plan 8;

# `&?BLOCK` must survive repeated calls of a named sub (the light call path
# used to skip the frame that holds it), and `&?ROUTINE` is the routine `&f`.
sub f { &?BLOCK.^name }
is f(), 'Sub', '&?BLOCK on the first call';
is f(), 'Sub', '&?BLOCK on the second call';
is f(), 'Sub', '&?BLOCK on the third call';

sub g($x) { &?BLOCK.^name }
is g(1), 'Sub', '&?BLOCK in a sub with a parameter, first call';
is g(2), 'Sub', '&?BLOCK in a sub with a parameter, second call';

sub h { &?ROUTINE === &h }
ok h(), '&?ROUTINE === &h';
ok h(), '&?ROUTINE === &h on the second call';
sub k { &?ROUTINE.WHICH eq &k.WHICH }
ok k(), '&?ROUTINE.WHICH eq &k.WHICH';
