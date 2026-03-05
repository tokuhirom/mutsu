use Test;

plan 18;

# Basic Z operator
is (<a b> Z <1 2>), <a 1 b 2>, 'non-meta zip produces expected result';
is (<a b> Z~ <1 2>), <a1 b2>, 'zip-concat produces expected result';
is (1,2 Z* 3,4), (3,8), 'zip-product works';
is (1, 2, 3 Z** 2, 4), (1, 16), 'zip-power works';

# Scalar arguments
is (1 Z* 3,4), (3), 'zip-product works with scalar left side';
is (1, 2 Z* 3), (3), 'zip-product works with scalar right side';

# Lazy range iteration (should not hang)
is (1..* Z** 1..*).[^5], (1**1, 2**2, 3**3, 4**4, 5**5), 'zip-power with lazy lists';
is (1..* Z+ (3, 2, 1, 0, -1)).[^5], (4, 4, 4, 4, 4), 'zip-plus with lazy range and list';

# Extending with * (Whatever)
is (<a b c d> Z 'x', 'z', *), <a x b z c z d z>, 'non-meta zip extends right argument ending with *';
is (1, 2, 3, * Z 10, 20, 30, 40, 50), (1, 10, 2, 20, 3, 30, 3, 40, 3, 50),
    'non-meta zip extends left argument ending with *';
is (2, 10, * Z 3, 4, 5, *).[^5], (2, 3, 10, 4, 10, 5, 10, 5, 10, 5),
    'non-meta zip extends two arguments ending with *';
is (<a b c d> Z~ 'x', 'z', *), <ax bz cz dz>, 'zip-concat extends right argument ending with *';
is (1, 2, 3, * Z+ 10, 20, 30, 40, 50), (11, 22, 33, 43, 53),
    'zip-plus extends left argument ending with *';

# [Z+] reduction
is ([Z+] (1, 2), (20, 10)), (21, 12), '[Z+] with two lists';

# Z-prefixed meta-operator in reduction context
is ((1,2) Z+ (20,10)), (21, 12), 'Z+ as direct infix produces expected result';
is ([21, 12] Z+ (100, 200)), (121, 212), 'Z+ with array left operand';

# Z with cmp
is (1,2 Zcmp 3,2,0), (Order::Less, Order::Same), 'zip-cmp works';

# Z with =>
is (<a b> Z=> (1, 2)), (a => 1, b => 2), 'zip with pair construction';
