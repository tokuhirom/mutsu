use Test;

# `.join` with no argument uses the empty string as the separator. A Range has
# no materialized backing list, so the 0-arg fast path previously fell back to
# the Range's space-separated gist ("1 2 3 4 5") instead of joining its
# elements ("12345"). Regression test for that.

plan 12;

is (1..5).join, "12345", "Int Range .join with no separator";
is ("a".."e").join, "abcde", "Str Range .join with no separator";
is (1..5).join("-"), "1-2-3-4-5", "Range .join with explicit separator";
is (1..5).join(""), "12345", "Range .join with empty separator";
is (1..5).join(", "), "1, 2, 3, 4, 5", "Range .join with comma separator";
is (1^..^5).join, "234", "exclusive Range .join";
is (1..1).join, "1", "single-element Range .join";
is (1..0).join, "", "empty Range .join";
is (1..3).reverse.join, "321", "reversed Range .join";
is (1.5..4.5).join, "1.52.53.54.5", "Rat-endpoint Range .join";
is (1..5).list.join, "12345", "Range.list .join still works";
is (1, 2, 3).join, "123", "plain List .join unaffected";
