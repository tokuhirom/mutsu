use v6;
use lib 't/lib';
use Test;
use L10N::Testish;

# A generated L10N module registers its slang from EXPORT by calling
# `$*LANG.define_slang`; it does not need to use Slangify itself.
plan 12;

is (1 both 1), 1, 'a localized logical infix is parsed';
is (6 quotient 3), 2, 'a localized multiplicative infix is parsed';
is (1 same 1), True, 'a localized comparison infix is parsed';
is (3 times 2), '33', 'a localized replication infix is parsed';
ok current.defined, 'a localized now term is parsed';
ok clock.defined, 'a localized time term is parsed';
ok chance >= 0, 'a localized rand term is parsed';
is 'abc'.length, 3, 'a core alias is applied to a method name';

sub accepts-in(:$in) { $in }
is accepts-in(:inside(7)), 7, 'a localized named argument is canonicalized';
is ('a a' ~~ m:globalized/a/).elems, 2,
    'a localized regex adverb is canonicalized';
is q:worded<a b>.List, ('a', 'b'),
    'a localized quote adverb is canonicalized';
my @values = <x>;
is (@values[0]:kvp).elems, 2,
    'a localized postcircumfix adverb is canonicalized';
