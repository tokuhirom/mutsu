use lib 't/lib';
use Test;

# A parametric role's body is run once per composition, and an `is native`
# `sub` in it applies its trait where the role was declared. Which package that
# is was read off the FIRST method of `role.methods`, a `HashMap`: on some runs
# it was a method the runtime synthesizes (a `handles` delegate, declared in
# GLOBAL), the routine was declared there, and the second construction of the
# role called its declared body (`{ * }`) instead of the C function -- "Type
# check failed for return value; expected Pointer but got Whatever". Twelve
# roles give twelve independent hash orders in one run.
#
# Every expectation was verified against Rakudo.

plan 28;

class Loader {
    has $.M;
    method install($name) { $!M = (require ::($name)); True }
    method build() { $!M.new.build }
}

my $l = Loader.new;
$l.install('NativeRoleBodies');

my @first = $l.build;
is @first.elems, 12, 'twelve roles were constructed';
ok @first.map(*.live).all, 'every first construction holds a C allocation';

my @second = $l.build;
ok @second.map(*.live).all, 'and so does every second one (the C function, not the declared body)';
ok @second.map(*.two).all == 2, 'the role methods are still there';

for @first.kv -> $i, $o {
    ok $o.release, "role {$i + 1}: its allocation is freed";
}
for @second.kv -> $i, $o {
    ok $o.release, "role {$i + 1}, second construction: freed too";
}
