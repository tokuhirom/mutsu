my class errno {
    method Str { "E!" }
    method answer { 42 }
}
my $proxy := Proxy.new(FETCH => -> $ { UNIT::errno }, STORE => -> $, $v { 1 });
my sub term:<lexical-errno>() is export is raw { $proxy }
my sub own-type() is export { UNIT::errno }
