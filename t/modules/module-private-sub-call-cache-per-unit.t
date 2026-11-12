use v6;
use Test;
use lib 't/lib';
use PrivateHelperCacheA;
use PrivateHelperCacheB;

# A module's package-less `sub` is lexical to its compilation unit, so the
# same bare name means a different routine in each unit that declares one.
# mutsu caches a resolved call by name; a private routine's entry is keyed by
# the unit it was resolved in, and the calls below interleave three units'
# `helper` (and two modules' `absent`) many times so that an entry served to
# the wrong unit would show up as a wrong answer. Each module's helper also
# creates a closure, which keeps it off the light call paths. Verified
# against Rakudo.

plan 8;

sub helper($n) { "main-$n" }

my @got;
for ^50 -> $i {
    @got.push: a-helper($i) == $i * 10;
    @got.push: b-helper($i) == ($i + 1) * 2;
    @got.push: helper($i) eq "main-$i";
}
ok @got.all, 'each unit keeps calling its own private helper';

is a-many(5), 100, 'a block inside the unit calls its own helper';
is b-helper(3), 8, 'the second unit still answers with its own';
is helper(3), 'main-3', "the loading scope's own routine is not displaced";
nok b-absent-any(), "a module's private `absent` is its own";

# The loading scope never declared these; the modules' helpers stay private.
nok (try EVAL 'absent(Any)').defined, 'a private helper is not visible to the loader';

is a-helper(7), 70, 'the first unit answers after the others ran';
is helper(1), 'main-1', 'and the loader again';
