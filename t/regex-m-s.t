use Test;
plan 3;

ok "abc" ~~ m/ab/, 'm// match';
my $_ = "abc";
is s/ab/xy/, "xyc", 's/// returns substituted string';
is $_, "xyc", 's/// updates $_';
