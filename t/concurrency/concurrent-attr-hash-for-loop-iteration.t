use Test;

# #12483: a `for` loop over a shared `has %!h` walked the live map's keys with
# nothing held while other threads inserted, which could rehash it mid-walk:
# SIGSEGV in `HashData::typed_key` (release build). The VM's `for` now walks
# the keys under the container stripe (ADR-0068 §16). This pins mutsu's
# memory-safety guarantee, not a Raku one: the count need only be a possible one.

plan 6;

my @words = (('a'..'z').list, (('a'..'z') X~ ('a'..'z')).list).flat.list;   # 702 keys

sub hammer(&body) {
    await (^3).map: { start { my $end = now + 0.6; while now < $end { body() } } };
}

class Bare   { has %!h; method go { %!h{@words.pick} = 1; my $n = 0; for %!h { $n++ }; $n } }
class Pointy { has %!h; method go { %!h{@words.pick} = 1; my $n = 0; for %!h -> $p { $n++ if $p.value }; $n } }
class KV     { has %!h; method go { %!h{@words.pick} = 1; my $n = 0; for %!h.kv -> $k, $v { $n++ }; $n } }
class Keys   { has %!h; method go { %!h{@words.pick} = 1; my $n = 0; for %!h.keys { $n++ }; $n } }
class Arr    { has @!a; method go { @!a.push(1); my $n = 0; for @!a { $n++ }; $n } }

for (Bare, 'for %h'), (Pointy, 'for %h -> $p'), (KV, 'for %h.kv'), (Keys, 'for %h.keys'),
    (Arr, 'for @a') -> ($class, $name) {
    my $o = $class.new;
    my $max = 0;
    hammer({ $max max= $o.go });
    ok $max > 0, "$name over a container three threads insert into does not crash";
}

# The live-pair semantics are untouched without threads.
my %h = a => 1;
for %h -> $p { %h{$p.key} = 5; is $p.value, 5, 'a for-loop pair still reads the live entry' }
