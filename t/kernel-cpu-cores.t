use Test;

# $*KERNEL.cpu-cores reports the number of available CPU cores as a positive Int.

plan 2;

isa-ok $*KERNEL.cpu-cores, Int, '$*KERNEL.cpu-cores returns an Int';
ok $*KERNEL.cpu-cores > 0, '$*KERNEL.cpu-cores is positive';

# vim: expandtab shiftwidth=4
