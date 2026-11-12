use v6;
use Test;

# One bare routine name called alternately from several packages resolves,
# in each, to that package's own routine. mutsu caches a resolved call per
# (name, callsite package); the loops below interleave the packages so that
# an entry served to the wrong package -- or one package's call evicting the
# other's -- would show up as a wrong answer. Verified against Rakudo.

plan 5;

package Alpha {
    our sub helper($n) { "alpha-$n" }
    our sub call($n) { helper($n) }
    our sub call-closure($n) { my &c = { helper($n) }; c() }
}

package Beta {
    our sub helper($n) { "beta-$n" }
    our sub call($n) { helper($n) }
}

class Gamma {
    our sub helper($n) { "gamma-$n" }
    method call($n) { helper($n) }
}

sub helper($n) { "main-$n" }

my @got;
for ^30 -> $i {
    @got.push: Alpha::call($i) eq "alpha-$i";
    @got.push: Beta::call($i) eq "beta-$i";
    @got.push: Gamma.call($i) eq "gamma-$i";
    @got.push: helper($i) eq "main-$i";
    @got.push: Alpha::call-closure($i) eq "alpha-$i";
}
ok @got.all, 'each package keeps calling its own routine';
is Alpha::call(1), 'alpha-1', 'first package after the loop';
is Beta::call(1), 'beta-1', 'second package after the loop';
is Gamma.call(1), 'gamma-1', 'a class body after the loop';
is helper(1), 'main-1', 'the mainline after the loop';
