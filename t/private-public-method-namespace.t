use Test;

# A private method `method !foo` and a public method `method foo` live in
# separate namespaces and must not collide. mutsu stored both under the bare
# name `foo` and the duplicate-method check ignored privacy, so declaring a
# private `!request` alongside a public (multi) `request` raised
# "Package '...' already has a method 'request'".

plan 5;

class A {
    method request(Str $m) { 'pub:' ~ self!request($m) }
    method !request(Str $m) { "priv:$m" }
}
is A.new.request('GET'), 'pub:priv:GET',
    'public and private method of the same name coexist and both dispatch';

# Public multi + private of the same name (the HTTP::Tiny shape).
class B {
    proto method request(|c) { {*} }
    multi method request(Int $x) { 'int:' ~ self!request($x) }
    multi method request(Str $x) { 'str:' ~ self!request($x) }
    method !request($x) { "p($x)" }
}
is B.new.request(5),   'int:p(5)', 'private + public multi (Int candidate) works';
is B.new.request('z'), 'str:p(z)', 'private + public multi (Str candidate) works';

# A genuine duplicate public non-multi must still error.
dies-ok { EVAL 'class C { method m {1}; method m {2} }' },
    'two public non-multi methods of the same name still error';

# Two private non-multi methods of the same name must still error.
dies-ok { EVAL 'class D { method !m {1}; method !m {2} }' },
    'two private non-multi methods of the same name still error';
