use Test;
use nqp;

plan 9;

sub container-name(Mu \value) { value.VAR.^name }
sub is-container(Mu \value) { nqp::iscont(value) }
sub raw-name(Mu $value is raw) { $value.VAR.^name }

is container-name([1]), 'Array', 'an Array literal binds as its own container';
is is-container([1]), 0, 'the sigilless Array literal has no Scalar wrapper';

my @array = 1;
is container-name(@array), 'Array', 'an Array variable still binds directly';

my $scalar = [1];
is container-name($scalar), 'Scalar', 'a scalar holding an Array retains its Scalar';
is is-container($scalar), 1, 'the caller scalar remains a container';

my \assigned = [1];
is assigned.VAR.^name, 'Array', 'a sigilless assignment holds the Array itself';

my \bound := [1];
is bound.VAR.^name, 'Array', 'a sigilless bind holds the Array itself';

is container-name((1, 2)), 'List', 'a List value remains a List';
is raw-name([1]), 'Array', 'an explicit raw parameter remains unchanged';
