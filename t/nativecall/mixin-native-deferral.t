use v6;
use Test;

# The builtin on a native value a role is mixed into is the candidate behind
# the role's method (DeferralEntry::Native, ADR-11276 slice 4).
plan 4;

role Loud { method uc() { callsame().Str ~ "!" } }
my $s = "abc" but Loud;
is $s.uc, 'ABC!', 'callsame from a role method on a Str reaches the builtin';

role Count { method elems() { callsame() + 100 } }
my @a = (1, 2, 3);
@a does Count;
is @a.elems, 103, 'callsame on an Array mixin';

role Peek { method AT-KEY($k) { "<" ~ callsame() ~ ">" } }
my %h = a => 1;
%h does Peek;
is %h<a>, '<1>', 'callsame from AT-KEY on a Hash mixin';

role Plain { method hello() { "hi" } }
is (5 but Plain).hello, 'hi', 'a role method with no builtin counterpart';
