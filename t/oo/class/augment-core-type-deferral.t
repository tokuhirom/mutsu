use v6;
use Test;
use MONKEY-TYPING;

# A method augmented onto a core type that defers with nextsame/callsame/
# nextwith/callwith reaches the builtin method as its last candidate (#10198).

plan 9;

augment class Str {
    multi method FatRat(Str:D:) {
        nextsame unless self eq "0";
        FatRat.new(0, 5)
    }
}

augment class Array {
    method sort(|c) { "sorted:" ~ callsame().join(",") }
    method head(|c) { nextwith(2) }
}

is "1.5".FatRat.raku, 'FatRat.new(3, 2)', 'nextsame reaches the core Str.FatRat';
is "0".FatRat.raku, 'FatRat.new(0, 1)', 'the augmented body still runs when it does not defer';
is-deeply "7/2".FatRat, FatRat.new(7, 2), 'core Str.FatRat parses a rational literal';

is [3, 1, 2].sort, 'sorted:1,2,3', 'callsame from an augmented Array.sort reaches the builtin';
is [3, 1, 2].sort({ $^b <=> $^a }), 'sorted:3,2,1', 'callsame forwards the original arguments';
is-deeply [1, 2, 3, 4].head.List, (1, 2), 'nextwith passes new arguments to the builtin';

my @plain = 5, 4;
is @plain.sort, 'sorted:4,5', 'the augmentation applies to every Array';

my @seen;
my @other = 9, 8;
is [2, 1].sort({ @seen.push(@other.sort); $^a <=> $^b }), 'sorted:1,2',
    'the builtin runs for the deferring receiver only';
is @seen[0], 'sorted:8,9', 'another receiver inside the builtin still reaches the augmentation';
