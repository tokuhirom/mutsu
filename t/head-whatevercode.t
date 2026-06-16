use Test;

# .head with a WhateverCode / Whatever argument keeps "all but the last N"
# elements (the callable is invoked with 0; count = len + result).

plan 18;

# WhateverCode *-N on a List
is (1,2,3,4,5).head(*-2).list, (1, 2, 3), 'List.head(*-2)';
is (1,2,3,4,5).head(*-0).list, (1, 2, 3, 4, 5), 'List.head(*-0) keeps all';
is (1,2,3,4,5).head(*-10).list, (), 'List.head(*-N) clamps to empty';

# WhateverCode on a Range
is (1..10).head(*-3).list, (1, 2, 3, 4, 5, 6, 7), 'Range.head(*-3)';
is (1..5).head(*-2).list, (1, 2, 3), 'Range.head(*-2)';

# WhateverCode on an Array (variable and literal)
my @a = 1..5;
is @a.head(*-2).list, (1, 2, 3), 'Array.head(*-2)';
is [10,20,30,40].head(*-1).list, (10, 20, 30), 'array literal .head(*-1)';

# WhateverCode on a string list
is <a b c d e>.head(*-2).list, ('a', 'b', 'c'), 'string list .head(*-2)';

# Non-subtractive WhateverCode: callable(0) result is offset from end
is (1,2,3,4,5).head(*/2).list, (1, 2, 3, 4, 5), 'head(*/2) -> callable(0)=0 keeps all';
is (1,2,3,4,5).head(*+1).list, (1, 2, 3, 4, 5), 'head(*+1) clamps to all';

# bare Whatever keeps everything
is (1,2,3).head(*).list, (1, 2, 3), 'head(*) keeps all';
is (1..4).head(*).list, (1, 2, 3, 4), 'Range.head(*) keeps all';

# explicit pointy block behaves like the WhateverCode
is (1,2,3,4,5).head(-> $n { $n - 2 }).list, (1, 2, 3), 'head(pointy block)';

# plain numeric counts are unaffected
is (1,2,3,4,5).head(2).list, (1, 2), 'plain head(2) still works';
is (1,2,3,4,5).head(2.5).list, (1, 2), 'head(2.5) truncates';
is (1,2,3,4,5).head(0).list, (), 'head(0) is empty';
is (1..10).head(3).list, (1, 2, 3), 'Range.head(3) still works';

# head with no argument returns the first element
is (1,2,3).head, 1, 'head with no arg';
