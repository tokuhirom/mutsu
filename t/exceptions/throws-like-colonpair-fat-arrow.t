use Test;

plan 1;

# Test::Time 0.0.2 uses this positional colonpair spelling in its test suite.
throws-like { die "Time isn't mocked yet" }, X::AdHoc, :message => "Time isn't mocked yet";
