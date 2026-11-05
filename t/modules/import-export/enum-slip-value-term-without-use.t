use Test;

# A value that a computed enum body slips in (`slip <SLB>`) is a term of the
# unit, also in a unit with no `use` of a module for the undeclared-name check
# to be skipped on: it was reported as `Undeclared name: SLB`.

plan 3;

enum Slipped (:30SLA, slip <SLB SLC>);
is SLB.value, 31, 'a slipped value counts on from the colonpair';
is SLC.value, 32, 'and so does the next one';
is SLA.value, 30, 'the colonpair value itself';
