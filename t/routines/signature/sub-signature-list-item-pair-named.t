use Test;

# A List's `.Capture` takes each Pair element as a named part, also one held
# in an item container (`('?', $pair)`, an element read from a variable).
# CSS::Specification's actions build `[$quant, $value]` and
# CSS::Specification::Compiler destructures it with `($quant!, *%term)`.

plan 5;

my $v = :keyw<thin>;

sub slurp-named(@ ($q, *%t)) { %t }
is-deeply slurp-named(('?', $v)), %(keyw => 'thin'), 'a slurpy hash collects it';

my $l = ('?', $v);
is-deeply slurp-named($l), %(keyw => 'thin'), 'through a variable holding the List';

sub one-named(@ ($q, :$keyw)) { $keyw }
is one-named(('?', $v)), 'thin', 'a named parameter binds it';

multi pick(@ ($q, :$keyw!)) { "named $keyw" }
multi pick(@ ($q, $r)) { 'positional' }
is pick(('?', $v)), 'named thin', 'dispatch sees it as named';

sub f(:occurs(@)! ($quant!, :$trailing, *%term)) { "$quant {%term.keys}" }
my @occurs = ['?', $v];
is f(:@occurs), '? keyw', 'an Array built from it';
