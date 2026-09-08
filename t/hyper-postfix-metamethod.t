use Test;

# `».` hypers ordinary method calls only. A METAMETHOD after it (`».^name`) is
# not distributed: rakudo applies it to the container. See GitHub issue #7643.
#
# Every assertion here was checked against rakudo, so the file passes there too.

plan 9;

# 1-3. The metamethod applies to the container, not to each element.
my @a = Int, Str;
is (@a>>.^name).raku, '"Array"', 'a hyper metamethod names the Array, not its elements';

is ((Int, Str)>>.^name).raku, '"List"', 'the same for a List';

my @b = 1, 2;
is (@b>>.^name).raku, '"Array"', 'and for an Array of instances';

# 4. The Unicode spelling behaves identically.
is (@b».^name).raku, '"Array"', 'the » spelling behaves the same';

# 5. Chained: the second metamethod applies to the first one's Str result.
is (@b>>.^name.^name).raku, '"Str"', 'a chained metamethod applies to the result';

# 6. The spelling this was found through (`.^mro>>.^name`).
is (Int.^mro>>.^name).raku, '"List"', '.^mro>>.^name names the mro list itself';

# 7-9. Controls: an ordinary method after `».` still distributes.
is (@b>>.Str).raku, '["1", "2"]', 'a plain method after ». still distributes';
is (@b>>.Str>>.chars).raku, '[1, 1]', 'two hypered plain methods still distribute';
my @c = <a b>;
is (@c>>.uc).raku, '["A", "B"]', 'and so does a hypered .uc';
