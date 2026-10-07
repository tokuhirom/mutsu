use v6;
use Test;

# From HTML::Tag: an immutable Map answers Nil for a missing key (a Hash
# answers Any), so `when %constant-map{$_}` does not match via `'x' ~~ Any`.

plan 7;

constant %sr = :checked, :disabled;
my $m = Map.new((:a,));
my %h = :a;

ok $m<b> === Nil, 'Map missing key is Nil';
ok %sr<id> === Nil, 'constant Map missing key is Nil';
ok %h<b> === Any, 'Hash missing key stays Any';

my @got = <id checked class>.map({
    when %sr{$_} { "sr" }
    default { "other" }
});
is @got.join(','), 'other,sr,other', 'when over a constant Map lookup';
is ($m<b> // 'dflt'), 'dflt', '// still sees it as undefined';
nok $m<b>:exists, ':exists is False';
is (:a).Map<zz>.gist, 'Nil', 'Pair.Map missing key';

# vim: expandtab shiftwidth=4
