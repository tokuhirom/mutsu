use Test;

plan 7;

# A `given` (a statement that nets a stack value) inside a LEAVE/KEEP phaser
# body must be sunk: it used to leave its value behind and replace the value
# of the block or routine (#15613).
is (do { my $v will leave { 1 } = 5; $v }), 5, '`will leave` on a variable does not replace the block value';
is (do { my $v = 5; LEAVE { given $v { 1 } }; $v }), 5, 'LEAVE { given ... } in a do block';
is (do { LEAVE { given 5 { 1 } }; 4 }), 4, 'LEAVE { given ... } with a literal tail';
sub f { LEAVE { given 5 { 1 } }; 4 }
is f(), 4, 'LEAVE { given ... } in a routine';
sub g { my $v = 8; KEEP { given $v { 2 } }; $v }
is g(), 8, 'KEEP { given ... } in a routine';
sub h { my $v will leave { 1 } = 7; $v }
is h(), 7, '`will leave` in a routine';
sub k { my $v will leave { 1 } = 7; return $v }
is k(), 7, 'and with an explicit return';
