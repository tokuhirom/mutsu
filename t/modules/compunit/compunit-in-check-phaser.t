use Test;

plan 4;

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
