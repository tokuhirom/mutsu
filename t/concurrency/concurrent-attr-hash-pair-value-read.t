use Test;

# #13277: `for %!h -> $p { ... $p.value ... }` hands out live entry refs, and
# every `$p.value` re-reads the shared map. That read took no container
# stripe, so it raced the other threads' inserts (a rehash freed the buckets
# under it): a rare SIGSEGV or heap corruption on a release build. This pins
# mutsu's memory-safety guarantee, not a Raku one: the count need only be a
# possible one.

plan 2;

my @words = (('a'..'z').list, (('a'..'z') X~ ('a'..'z')).list).flat.list;   # 702 keys

class Pointy { has %!h; method go { %!h{@words.pick} = 1; my $n = 0; for %!h -> $p { $n++ if $p.value }; $n } }
class PointyKey { has %!h; method go { %!h{@words.pick} = 1; my $n = 0; for %!h -> $p { $n++ if $p.key.chars && $p.value }; $n } }

for Pointy, PointyKey -> $class {
    my $o = $class.new;
    my $max = 0;
    await (^6).map: { start { my $end = now + 0.8; while now < $end { $max max= $o.go } } };
    ok $max > 0, "{$class.^name}: reading \$p.value while six threads insert does not crash";
}
