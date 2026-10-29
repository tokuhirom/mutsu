use Test;
use lib 't/lib';
use ProbedOpPrecedence;

# The module hands its operator out through `OUR.WHO.BIND-KEY ... for Ops::`
# in an export stash, so the importer's parse learns it from a run-time probe.
# The probe must carry `is looser<...>` too: `...` is looser than the comma,
# so the operator takes the whole comma list as its right operand.

plan 2;

is (1 gg 6, 7), '1|2', 'an operator probed from an export stash keeps its precedence trait';
is (1 gg 6), '1|1', 'and still takes a single operand';
