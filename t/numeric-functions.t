use Test;
plan 10;

is-approx sin(0), 0, "sin 0 equals 0";
is-approx cos(0), 1, "cos 0 equals 1";
is-approx tan(0), 0, "tan 0 equals 0";
is-approx asin(0), 0, "asin 0 equals 0";
is-approx acos(1), 0, "acos 1 equals 0";
is-approx atan(1), 0.7853981633974483, "atan 1 equals pi/4";
is-approx atan2(1, 1), 0.7853981633974483, "atan2(1,1) equals pi/4";
is-approx log(8, 2), 3, "log base 2 of 8";
is-approx log(2), 0.6931471805599453, "log natural of 2";
is truncate(3.9), 3, "truncate removes fractional part";
