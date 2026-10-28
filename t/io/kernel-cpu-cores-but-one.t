use Test;

# `Kernel.cpu-cores-but-one` is an undocumented rakudo `is-implementation-detail`
# method that the ParaSeq distribution calls at load time. Added on the
# maintainer's per-issue approval (#15315); not a precedent.

plan 5;

isa-ok Kernel.cpu-cores-but-one, Int, 'Kernel.cpu-cores-but-one is an Int';
isa-ok $*KERNEL.cpu-cores-but-one, Int, '$*KERNEL.cpu-cores-but-one is an Int';
is Kernel.cpu-cores-but-one, (Kernel.cpu-cores - 1 max 1), 'it is cpu-cores - 1, at least 1';
ok Kernel.cpu-cores-but-one >= 1, 'never below one';
is $*KERNEL.^can('cpu-cores-but-one').elems, 1, '.^can sees the method';
