use Test;
plan 3;

ok "abc" ~~ m/ab/, 'm// match';
my $_ = "abc";
ok s/ab/xy/, 's/// returns truthy on match';
is $_, "xyc", 's/// updates $_';
