use v6.c;
use Test;

# 6.c "precedes"/"succeeds" baggy set operators: (<+) ≼ (>+) ≽.
# Deprecated aliases of (<=)/⊆ and (>=)/⊇ (removed in 6.d); they compare
# Bag/Mix multiplicities, not just element presence.

plan 16;

my $bb = bag <a a b>;       # a:2 b:1
my $b2 = bag <a b>;         # a:1 b:1
my $b3 = bag <a a a b>;     # a:3 b:1

# (<+) / ≼ : baggy subset (multiplicity-wise <=)
ok  $b2 (<+) $bb, 'bag (<+) : proper baggy subset';
ok  $b2 ≼    $bb, 'bag ≼ : proper baggy subset';
ok  $bb (<+) $bb, 'bag (<+) : subset of itself';
nok $b3 (<+) $bb, 'bag (<+) : too many copies is NOT a subset';
nok $b3 ≼    $bb, 'bag ≼ : too many copies is NOT a subset';

# (>+) / ≽ : baggy superset
ok  $bb (>+) $b2, 'bag (>+) : proper baggy superset';
ok  $bb ≽    $b2, 'bag ≽ : proper baggy superset';
ok  $bb (>+) $bb, 'bag (>+) : superset of itself';
nok $bb (>+) $b3, 'bag (>+) : missing copies is NOT a superset';

# Mixes (fractional weights)
my $m1 = (a => 1.5, b => 2).Mix;
my $m2 = (a => 1.0, b => 1).Mix;
ok  $m2 (<+) $m1, 'mix (<+) : baggy subset with weights';
ok  $m2 ≼    $m1, 'mix ≼ : baggy subset with weights';
nok $m1 (<+) $m2, 'mix (<+) : larger weights not a subset';
ok  $m1 (>+) $m2, 'mix (>+) : baggy superset with weights';

# Sets degrade to multiplicity 1
my $s1 = set <a b>;
my $s2 = set <a b c>;
ok  $s1 (<+) $s2, 'set (<+) : subset';
ok  $s1 ≼    $s2, 'set ≼ : subset';
nok $s2 ≼    $s1, 'set ≼ : superset is not a subset';
