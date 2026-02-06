use Test;
plan 12;

ok "abc" eq "abc", "eq true";
ok "abc" ne "xyz", "ne true";
ok not ("abc" eq "xyz"), "eq false";
ok not ("abc" ne "abc"), "ne false";

ok "abc" lt "xyz", "lt true";
ok "xyz" gt "abc", "gt true";
ok "abc" le "abc", "le equal";
ok "abc" le "xyz", "le less";
ok "xyz" ge "xyz", "ge equal";
ok "xyz" ge "abc", "ge greater";

ok not ("xyz" lt "abc"), "lt false";
ok not ("abc" gt "xyz"), "gt false";
