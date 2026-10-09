use Test;

# #12456: iteration and callback methods (`.sort`, `.map`, `.gist`) of a shared
# `has %!h` walked the backing map with nothing held while other threads
# inserted: SIGSEGV or an OOM abort in `hash_key_decode` (2 of 4 runs for
# `.sort`/`.map`, 4 of 4 for `.gist`, release build). They now run on a shallow
# snapshot taken under the container stripe (ADR-0068 §16). This pins mutsu's
# memory-safety guarantee, not a Raku one: the answer need only be a possible one.

plan 4;

my @words = (('a'..'z').list, (('a'..'z') X~ ('a'..'z')).list).flat.list;   # 702 keys

sub hammer(&body) {
    await (^3).map: { start { for ^3000 { body() } } };
}

class Sorter { has %!h; method go { %!h{@words.pick} = 1; %!h.sort.elems } }
class Mapper { has %!h; method go { %!h{@words.pick} = 1; %!h.map({ .key }).elems } }
class Gister { has %!h; method go { %!h{@words.pick} = 1; %!h.gist.chars } }

for (Sorter, '.sort'), (Mapper, '.map'), (Gister, '.gist') -> ($class, $name) {
    my $o = $class.new;
    my $max = 0;
    hammer({ $max max= $o.go });
    ok $max > 0, "$name on a hash three threads insert into does not crash";
}

# Single-threaded semantics are untouched: .map over an array still aliases.
my @a = 1, 2, 3;
@a.map({ $_++ });
is-deeply @a, [2, 3, 4], '.map still aliases elements of an unshared array';
