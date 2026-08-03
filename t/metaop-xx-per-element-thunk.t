use Test;

# `xx` re-evaluates its left operand once per repetition. Under a cross or zip
# meta-op that re-evaluation is *per element* of the left list: `($i++, 100) Xxx
# 3` is `((0,1,2), (100,100,100))`, one Seq per left element, not three copies
# of the whole list. mutsu used to thunk the left side as a whole and repeat it,
# which transposed the result and (for `Zxx`) over-ran the side effects.
#
# A left side that is not a list literal is a single already-evaluated value in
# Rakudo too, so it must NOT be re-evaluated.

plan 19;

# --- Xxx: shape ----------------------------------------------------------

is ((1, 2) Xxx 3).elems, 2, 'Xxx yields one Seq per left element';
is ((1, 2) Xxx 3).map(*.join(',')).join('|'), '1,1,1|2,2,2',
    'each left element is repeated on its own';
is ((1, 2) Xxx (3, 4)).map(*.join(',')).join('|'),
    '1,1,1|1,1,1,1|2,2,2|2,2,2,2',
    'left element outer, count inner';

# --- Xxx: per-element re-evaluation ---------------------------------------

{
    my $i = 0;
    is (($i++, 100) Xxx 3).map(*.join(',')).join('|'), '0,1,2|100,100,100',
        'only the element with the side effect is re-evaluated';
    is $i, 3, '... exactly once per repetition';
}

{
    my $t = 0;
    is (($t++, 5) Xxx (2, 3)).map(*.join(',')).join('|'), '0,1|2,3,4|5,5|5,5,5',
        'a thunked element re-runs for every count it is crossed with';
    is $t, 5, '... 2 + 3 times in total';
}

{
    my $s = 0;
    is (($s++, $s++) Xxx 2).map(*.join(',')).join('|'), '0,1|2,3',
        'two thunked elements run element-major';
    is $s, 4, '... two evaluations each';
}

# --- a non-literal left side is evaluated once ----------------------------

{
    my $p = 0;
    is (($p++ Xxx 3).map(*.join(',')).join('|')), '0,0,0',
        'a scalar left operand is evaluated once and repeated';
    is $p, 1, '... with a single side effect';
}

{
    my $m = 0;
    my @l = ($m++, 100);
    is (@l Xxx 3).map(*.join(',')).join('|'), '0,0,0|100,100,100',
        'an array variable left side is a plain value list';
    is $m, 1, '... whose elements were already evaluated';
}

{
    my $u = 0;
    is (($u++, 5).list Xxx 2).map(*.join(',')).join('|'), '0,0|5,5',
        'a method call on the left side is evaluated once';
    is $u, 1, '... with a single side effect';
}

# --- a list element keeps its identity ------------------------------------

{
    my @a = 1, 2;
    is ((@a, 5) Xxx 2).map({ .map({ .elems // 1 }).join(',') }).join('|'), '2,2|1,1',
        'an array element is repeated whole, not flattened into the cross';
}

# --- Zxx pairs element i with count i -------------------------------------

{
    my $v = 0;
    is (($v++, 5) Zxx (2, 3)).map(*.join(',')).join('|'), '0,1|5,5,5',
        'Zxx repeats each element by its own count';
    is $v, 2, '... running element 0 only as often as its own count';
}

is ((1, 2) Xxx 0).map(*.elems).join(','), '0,0', 'a zero count yields empty Seqs';
