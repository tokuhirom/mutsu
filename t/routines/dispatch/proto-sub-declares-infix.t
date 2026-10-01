use v6.c;
use Test;

# Found via the BinaryHeap ecosystem distribution: a lone
# `proto sub infix:<op>($, $) {*}` declares the operator for the parser even
# when no `multi` candidate follows. Parse-level only: calling the operator
# through a same-named `&infix:<op>` parameter is tracked separately.

plan 2;

proto sub infix:<precedes>($, $) {*}

role Heap[&infix:<precedes> = * cmp * == Less] {
    method t($a, $b) { $a precedes $b }
    method u($a, $b) { self && $a precedes $b ?? 'y' !! 'n' }
}

ok Heap.^name eq 'Heap', 'role body using a proto-declared infix parses';

sub never-called($a, $b) { $a precedes $b }
ok &never-called.defined, 'sub body using a proto-declared infix parses';
