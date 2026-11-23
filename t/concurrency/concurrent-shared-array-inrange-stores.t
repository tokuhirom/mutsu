use Test;

# ADR-18752 (#8069): an in-range element store into a celled array that
# several threads share takes the ADR-0068 stripes on the shared side and
# swaps the slot word. Stores to disjoint slots no longer exclude each other;
# they must still exclude structural writes (push) and live-node reads
# (iteration), and a store that races another on one slot must leave one of
# the two values, never a torn or freed word.

plan 6;

# Disjoint slots: every worker owns 16 indices; the survivors are fixed.
{
    my @backing = 0 xx 64;
    my @slots := @backing;
    await (^4).map: -> $t {
        start {
            for ^5000 -> $i { @slots[$t * 16 + ($i +& 15)] = $t * 100000 + $i }
        }
    };
    my @want = (^4).map(-> $t { (^16).map(-> $j { $t * 100000 + (^5000).grep(* +& 15 == $j).tail }) }).flat;
    is-deeply @slots.List, @want.List, 'disjoint-slot stores: every final value is visible after await';
}

# One slot, four writers: the survivor is one of the values written last.
{
    my @backing = 0 xx 8;
    my @one := @backing;
    await (^4).map: -> $t { start { for ^5000 { @one[3] = $t } } };
    ok @one[3] ~~ 0..3, 'same-slot stores leave one of the written values';
    is-deeply @one[0,1,2,4,5,6,7].List, (0 xx 7).List, 'the other slots are untouched';
}

# Stores racing pushes and iteration on one array.
{
    my @backing = 0 xx 32;
    my @mixed := @backing;
    my $stop = False;
    my @readers = (^2).map: {
        start {
            my $bad = 0;
            until $stop {
                for @mixed -> $v { $bad++ unless $v ~~ Int }
                $bad++ if @mixed.elems < 32;
            }
            $bad
        }
    };
    my $pusher = start { for ^2000 { @mixed.push(-1) } };
    await (^3).map: -> $t { start { for ^5000 -> $i { @mixed[$t * 8 + ($i +& 7)] = $i } } };
    await $pusher;
    $stop = True;
    is ([+] await @readers), 0, 'readers iterating during stores and pushes see only Ints and no shrink';
    is @mixed.elems, 2032, 'every push landed';
    is-deeply @mixed[24..31].List, (0 xx 8).List, 'slots no writer owns keep their value';
}
