use v6;
use Test;

# A `my` declared inside a map/grep/first block is the block's own fresh
# binding. It must NOT leak into a same-named lexical of the calling frame
# (visible through the flattened env). zef hit this in Zef::Distribution:
# `provides-specs` runs `.map({ my $spec = ...; $spec })` and the enclosing
# `provides-spec-matcher($spec)` parameter got clobbered by the block-local
# value, so `contains-spec` matched a dist against ITSELF and silently
# dropped JSON::OptIn from the prereq list (only on the first, cache-cold
# call — the cached second call skipped the map and behaved).

plan 6;

class Sp {
    has $.name;
    method m($o) { $.name eq $o.name }
}

class Dist {
    has $.name;
    has @!cache;
    method specs {
        return @!cache if @!cache.elems;
        my @s = ($.name,).map({
            my $spec = Sp.new(name => $_);
            $spec;
        });
        @!cache = @s;
        return @!cache;
    }
    method pm($spec) {
        so self.specs.first({ $_.m($spec) })
    }
}

my $d = Dist.new(name => 'A');
nok $d.pm(Sp.new(name => 'B')), 'first call (cache-cold, map runs): no false match';
nok $d.pm(Sp.new(name => 'B')), 'second call (cache-hit): still no false match';
ok $d.pm(Sp.new(name => 'A')), 'a genuine match still matches';

class D2 {
    method specs { ('A',).map({ my $spec = Sp.new(name => $_); $spec }).eager }
    method pm($spec) { self.specs; $spec.name }
}
is D2.new.pm(Sp.new(name => 'B')), 'B',
    'a map-block my does not clobber the caller method param directly';

# The legitimate captured-outer mutation writeback must survive.
my $sum = 0;
(1, 2, 3).map({ my $t = $_ * 2; $sum += $t }).eager;
is $sum, 12, 'captured-outer mutation from a map block still writes back';

my $x = 1;
(1,).map({ $x = 5 }).eager;
is $x, 5, 'plain captured assignment from a map block still writes back';
