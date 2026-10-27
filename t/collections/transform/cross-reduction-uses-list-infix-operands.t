use Test;

plan 8;

is-deeply ([X] (gather { take 1; take 2 }), (3, 4)).Array.raku, '[(1, 3), (1, 4), (2, 3), (2, 4)]',
    '[X] reads a plain gather operand';
is-deeply ([Z] (gather { take 1; take 2 }), (3, 4)).Array.raku, '[(1, 3), (2, 4)]',
    '[Z] reads a plain gather operand';
is-deeply ([X] (lazy gather { take 1; take 2 }), (3, 4)).head(4).Array.elems, 4,
    '[X] reads a lazy-marked gather operand';
is-deeply ([X] (1, 2, 3).item, (1..*)).head(3).Array.raku, '[(1, 1), (1, 2), (1, 3)]',
    '[X] over an itemized list and an infinite range stays lazy and flat';
is-deeply ([X] (lazy gather { take 1; take 2 }).item, (1..*)).head(3).Array.raku, '[(1, 1), (1, 2), (1, 3)]',
    '[X] over an itemized lazy gather and an infinite range';
is-deeply ([X] (1, 2), (3, 4), (5, 6)).Array.elems, 8, '[X] with three operands is one n-ary product';
is-deeply ([X] (1, 2), (3, 4), (5, 6)).head(1).Array.raku, '[(1, 3, 5),]', 'and its tuples are flat';
is-deeply ([Z] (1, 2), (3, 4), (5, 6)).Array.raku, '[(1, 3, 5), (2, 4, 6)]', '[Z] with three operands';
