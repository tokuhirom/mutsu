use Test;

throws-like '(1, 2, 3)[0]:delete', X::AdHoc,
    message => 'Can not remove elements from a List',
    'a literal List refuses deletion';

throws-like 'my $list := (1, 2, 3); $list[1]:delete', X::AdHoc,
    message => 'Can not remove elements from a List',
    'a bound List also refuses deletion';

throws-like 'say (1, 2, 3)[1, 2, :c(3)]', X::Multi::NoMatch,
    'a Pair in a positional slice cannot coerce to Int';

throws-like 'say (1, 2, 3)[(0, :c(3))]', X::Multi::NoMatch,
    'a Pair in a nested positional slice cannot coerce to Int';

my @mutable = 1, 2, 3;
is @mutable[0]:delete, 1, 'a mutable Array still permits deletion';
throws-like 'my @a = 1, 2, 3; say @a[1, :c(3)]', X::Multi::NoMatch,
    'a Pair in a mutable Array slice cannot coerce to Int';

done-testing;
