use Test;
use nqp;

# ADR-14162: the `_nd` ("no decont") nqp ops look at the container itself.
# AttrX::Mooish takes an attribute's `auto_viv_container`, tests it with
# `isconcrete_nd`, clones it with `clone_nd` and assigns through the clone.

plan 8;

class A { has $.x; has @.y; has %.z }

my Mu $aviv := A.^attributes[0].auto_viv_container;
ok nqp::iscont($aviv), 'a scalar attribute\'s auto_viv_container is a container';
ok nqp::isconcrete_nd($aviv), 'a container is concrete even though it holds a type object';

my $clone := nqp::clone_nd($aviv);
ok nqp::iscont($clone), 'clone_nd of a container is a container';
$clone = 5;
is $clone, 5, 'the clone can be assigned';
nok nqp::isconcrete_nd($aviv.VAR.WHAT), 'the type object of the container is not concrete';

is A.^attributes[1].auto_viv_container.^name, 'Array', '@ attribute';
is A.^attributes[2].auto_viv_container.^name, 'Hash', '% attribute';
my Mu $other := A.^attributes[0].auto_viv_container;
$other = 9;
is $aviv.VAR.^name, 'Scalar', 'each call hands out a fresh container';

done-testing;
