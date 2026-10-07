use v6;
use MONKEY-TYPING;
use Test;

# The builtin behind an augmented method is a candidate of the frame
# (DeferralEntry::Native, ADR-11276 slice 4).
plan 3;

augment class Array { method sort(|c) { "sorted:" ~ callsame().join(",") } }
is [3, 1, 2].sort, 'sorted:1,2,3', 'callsame reaches the builtin sort';

augment class Str { multi method FatRat(Str:D:) { nextsame } }
is "1.5".FatRat, 1.5, 'nextsame from a multi method reaches the builtin';

augment class Str { method shout() { self.uc } }
is "ab".shout, 'AB', 'an augmented method with no builtin counterpart';
