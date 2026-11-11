unit module ConstantOuterScope;
my constant DAYS = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
constant MAXP = 1200;
our sub days-in(Int $m --> Int) is export { my Int $limit = DAYS[$m - 1]; $limit }
our sub max-periods(--> Int) is export { MAXP }
