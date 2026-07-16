use v6;
use Test;

plan 13;

# `take` of a Slip / Empty flattens into the gather; `take` of a nested
# `gather` is reified so its content survives; `flat` of a finite gather
# descends into its nested lazy elements. Root cause of the P62 binary-tree
# walk (roast/integration/99problems-61-to-70.t internals/atlevel).

# (1) take Empty / take slip flatten (add zero / N elements)
is (gather { take 9; take Empty; take Empty }).elems, 1, 'take Empty adds nothing';
is (gather { take slip(1, 2) }).elems, 2, 'take slip(1,2) flattens to two elements';
is (gather { take 0; take Empty; take slip(1, 2) }).join(','), '0,1,2',
    'mixed take Empty / take slip';

# (2) take of a nested gather keeps its content
is (gather { take 5; take gather { take 1; take 2 } }).flat.join(','), '5,1,2',
    'take of a nested gather is reified and flattens';
is (gather { take gather { take 1; take 2 } }).flat.join(','), '1,2',
    'take of a nested gather (no lead)';

# (3) flat of a finite gather descends into nested lazy elements
is (flat gather { take 1; take gather { take 2; take 3 } }).join(','), '1,2,3',
    'flat of a gather containing a nested gather';

# (4) P62 internals: collect internal nodes of a binary tree
my $tree = ['A', ['B', ['C', Any, Any], ['D', Any, Any]], ['E', Any, Any]];
sub internals($t) {
    return Empty unless defined($t);
    gather {
        take $t[0] if defined($t[1]) and defined($t[2]);
        take internals($t[1]);
        take internals($t[2]);
    }
}
is-deeply [flat internals($tree)], ['A', 'B'], 'P62 internals() collects internal nodes';

# (5) P62B atlevel: collect nodes at a given level
sub atlevel($t, $level) {
    return Empty unless defined($t);
    return @($t[0]) if $level == 1;
    gather {
        take atlevel($t[1], $level - 1);
        take atlevel($t[2], $level - 1);
    }
}
is-deeply [flat atlevel($tree, 1)], ['A'], 'atlevel level 1';
is-deeply [flat atlevel($tree, 2)], ['B', 'E'], 'atlevel level 2';
is-deeply [flat atlevel($tree, 3)], ['C', 'D'], 'atlevel level 3';

# (6) a plain finite non-nested gather still flattens correctly
is (flat gather { take 1; take 2; take 3 }).join(','), '1,2,3',
    'flat of a plain finite gather';

# (7) flat still propagates .is-lazy for a non-gather infinite lazy list —
# forcing must be limited to finite gather coroutines (S02-types/array.t).
is-deeply flat(42 xx *).is-lazy, True,  'flat of an infinite lazy list stays lazy';
is-deeply flat(42 xx 1).is-lazy, False, 'flat of a finite list is not lazy';
