use Test;

plan 7;

class H {
    method a { my sub gs($x) { "a$x" }; anon method ($y) { gs($y) } }
    method b { my sub gs($x) { "b$x" }; anon method ($y) { gs($y) } }
}
my $a = H.new.a;
my $b = H.new.b;
is $a.(0, 1) ~ $b.(0, 2), 'a1b2', 'each escaped closure calls the my sub of its own method';
is $a.(0, 3), 'a3', 'the first closure still calls its own sub after the second ran';

class S {
    method counter { my sub next-id() { state $n = 0; ++$n }; next-id() ~ next-id() }
    method other   { my sub next-id() { 'o' }; next-id() }
}
is S.new.counter, '12', 'state variable of a live my sub works';
is S.new.other, 'o', 'a same-named my sub in another method';
is S.new.counter, '12', 'repeated call of the first method';

class K {
    method a { my sub gs($x) { "a$x" }; sub ($y) { gs($y) } }
    method b { my sub gs($x) { "b$x" }; sub ($y) { gs($y) } }
}
my ($ka, $kb) = K.new.a, K.new.b;
is $ka(1) ~ $kb(2), 'a1b2', 'an anonymous sub escaping a method calls its own my sub';
is $ka(3), 'a3', 'the first anonymous sub keeps its own my sub';
