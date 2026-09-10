use Test;
plan 14;

is 5.succ, 6, "Int succ";
is 5.pred, 4, "Int pred";

is 5.so, True, "Int.so truthy";
is 0.so, False, "0.so falsy";
is "hello".so, True, "Str.so truthy";

is True.not, False, "Bool.not";
is False.not, True, "False.not";

is 9.sqrt, 3, "sqrt of perfect square";
is 2.5.floor, 2, "floor";
is 2.3.ceiling, 3, "ceiling";
is 2.5.round, 3, "round up";
is 2.4.round, 2, "round down";

is 255.base(16), "FF", "base 16";
is 7.base(2), "111", "base 2";
