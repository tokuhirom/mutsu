use Test;

# A pure positional list (List/Array/Seq) in numeric context coerces to its
# element count, so the numeric comparison operators compare elem counts, NOT
# element-wise (that is `cmp`). Previously `<=>` compared lists element-wise and
# `==` compared two same-typed lists structurally.

plan 14;

# <=> compares element counts.
is (1, 2, 3) <=> (1, 2, 4), Same, '<=> on equal-length lists is Same';
is [1, 2, 3] <=> [4, 5], More, '<=> on Arrays compares elems (3 vs 2)';
is <a b c> <=> <d e>, More, '<=> on string lists compares elems';
is (1, 2, 3) <=> (1, 2, 4, 5), Less, '<=> shorter list is Less';

# == / != compare element counts.
ok (1, 2, 3) == (1, 2, 4), '== on equal-length lists is True';
nok (1, 2, 3) == (1, 2), '== on different-length lists is False';
ok (1, 2, 3) != (1, 2), '!= on different-length lists is True';

# Relational operators (already worked) stay correct.
ok (1, 2, 3) < (4, 5, 6, 7), '< compares elems (3 < 4)';
ok (1, 2, 3) <= (1, 2, 4), '<= on equal-length lists';
ok (1, 2, 3) >= (4, 5, 6), '>= on equal-length lists';

# Arithmetic in numeric context (already worked) stays correct.
is (1, 2, 3) + (4, 5), 5, '+ adds elem counts';
is (1, 2, 3) * 2, 6, '* multiplies elem count';

# Scalar numerics are unaffected.
is 5 <=> 2, More, 'scalar <=> still works';
is 1.5 <=> 2.5, Less, 'rational <=> still works';
