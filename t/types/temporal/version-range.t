use v6;
use Test;

# `v1..v2` is a Range of Version literals, not one malformed version, and a
# Version smartmatches a Range of Versions by Version ordering (mutsu#14870,
# Pakku::Spec's `:ver(v1..v2)`).
plan 12;

my $r = v1..v2;
is $r.raku, 'v1..v2', 'v1..v2 is a range of two versions';
isa-ok $r, Range, 'it is a Range';
ok v1.5 ~~ $r, 'a version inside the range matches';
ok Version.new('1.5') ~~ $r, 'a Version.new inside the range matches';
nok v0.1 ~~ $r, 'a version below the range does not match';
nok v3 ~~ $r, 'a version above the range does not match';
ok v1 ~~ $r, 'the lower end is included';
nok v1 ~~ (v1^..v2), 'an excluded lower end is not';
ok v1.5 ~~ (v1..*), 'an open upper end';
nok v0.5 ~~ (v1..*), 'below an open upper end';
nok Version.new('0.1') ~~ (1..*), 'a Version outside an Int range does not match';
is (v1.2.3).Str, '1.2.3', 'a dotted version literal still parses whole';
