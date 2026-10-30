use Test;

plan 6;

# `$*CU` is the unit's RakuAST::CompUnit in a CHECK phaser and in a function
# a CHECK phaser calls (static's `CHECK fail-on-mismatch()`, #15610), and a
# Failure once the unit is compiled.
my @seen;
sub name-of($cu = $*CU) { $cu.^name }
BEGIN @seen.push: 'begin ' ~ $*CU.^name;
CHECK @seen.push: 'check ' ~ $*CU.^name;
CHECK @seen.push: 'call ' ~ name-of();

is @seen[0], 'begin RakuAST::CompUnit', 'BEGIN';
is @seen[1], 'call RakuAST::CompUnit', 'a CHECK phaser calling a sub that defaults to $*CU';
is @seen[2], 'check RakuAST::CompUnit', 'a CHECK phaser';
ok $*CU ~~ Failure, 'a Failure at run time';

# A declaration after the CHECK phaser is hoisted ahead of it; the unit's
# compile phase still lasts until the phaser has run.
my $declared-after = 1;
is @seen.elems, 3, 'a later declaration does not end the compile phase early';

# A CHECK phaser sees the unit's subs (rakudo installs them while compiling).
my $seen-sub;
sub unit-sub() { 42 }
CHECK $seen-sub = unit-sub();
is $seen-sub, 42, 'a CHECK phaser can call a sub declared after it';
