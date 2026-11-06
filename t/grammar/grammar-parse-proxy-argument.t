use Test;

# From Cooklang: `Recipe.subparse($!recipe, ...)` where `$!recipe` is an
# `is mooish` attribute, i.e. a Proxy. The grammar parses what the Proxy
# FETCHes, not the Proxy's own name.
plan 6;

grammar G { token TOP { \w+ } }
class K {
    has $!store = "Banana cake";
    has $.p;
    method TWEAK { $!p := Proxy.new(FETCH => -> $ { $!store }, STORE => -> $, $v { $!store = $v }) }
    method sub1 { G.subparse($!p).Str }
    method sub2 { G.subparse($!p, actions => Any).Str }
    method sub3 { my $s = $!p; G.subparse($s).Str }
    method par  { G.parse($!p, :rule<TOP>) }
    method sub4 { G.subparse($!p).to }
    method set($v) { $!store = $v }
}
my $k = K.new;
is $k.sub1, 'Banana', 'subparse of a Proxy attribute';
is $k.sub2, 'Banana', 'with an :actions argument';
is $k.sub3, 'Banana', 'a copy of the value (control)';
is $k.sub4, 6, '.to counts the fetched text';
$k.set("Pear tree");
is $k.sub1, 'Pear', 'a stored value is what gets parsed';
nok $k.par.defined, 'parse (anchored) of the fetched text fails as text, not as "Proxy"';
