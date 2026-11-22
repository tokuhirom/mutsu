use Test;

# #18667: an indexed store through a chain of container references
# (`$alias` bound to `$holder`, which holds an Array) descends hop by hop.
# Each hop after the first used to be read through a pointer escaped from the
# previous cell's lock, so a thread re-assigning `$holder` while another stored
# through `$alias[$i]` raced on the freed slot (a TSan report, and a possible
# use-after-free). The descent now reads every hop under that cell's guard.
# The program below synchronizes with a Lock, so (ADR-12814) it must stay
# memory-safe and lose no element whatever the interleaving.

plan 3;

my $holder = [0 xx 8];
my $alias := $holder;
my $lock = Lock.new;

my @writers = (^4).map: -> $t {
    start {
        for ^2000 -> $n {
            $lock.protect: { $alias[($t * 2 + $n) % 8] = $n };
        }
        True
    }
};
my $swapper = start {
    for ^500 {
        $lock.protect: { $holder = [0 xx 8] };
    }
    True
};

ok (await @writers).all.so, 'every writer finished';
ok (await $swapper), 'the re-assigning thread finished';
is $alias.elems, 8, 'the alias still sees an 8-element Array';
