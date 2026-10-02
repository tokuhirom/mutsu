use Test;

# `.raku` of a lazy Seq reifies a 100-element prefix, glues `...` to it when
# more remain, and ends in `.lazy.Seq` (rakudo's Seq.raku). A type-check
# message that shows such a Seq reuses the same text.

plan 6;

my $inf = (1..*).map({ $_ * 2 }).raku;
ok $inf.starts-with('(2, 4, 6, 8, 10, 12,'), 'infinite map pipe starts with its prefix';
ok $inf.ends-with(', 198, 200...).lazy.Seq'), 'ends after 100 elements with ... and .lazy.Seq';
is $inf.comb(/','/).elems, 99, 'exactly 100 elements shown';

is (1..*).map({ $_ * 2 }).perl.substr(0, 20), '(2, 4, 6, 8, 10, 12,', '.perl agrees';
my @a = 1..*;
is @a.raku, '[...]', 'a lazy Array keeps the [...] placeholder';

role R {}
class C { has R $.r }
try C.new(:r((1..*).map({ $_ * 2 })));
like $!.message, /'got Seq ((2, 4, 6, 8, 10, 12,'/, 'type-check message carries the lazy prefix';
