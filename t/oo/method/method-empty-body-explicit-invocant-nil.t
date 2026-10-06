use Test;

plan 5;

class D {
    method a(D:D: Int $x) {}
    method b(Int $x) {}
    method c(D:D:) {}
    method d($s:) {}
}

# An empty method body returns Nil whatever the shape of the signature (#11861).
is-deeply D.new.a(1), Nil, 'empty body, typed explicit invocant and a parameter';
is-deeply D.new.b(1), Nil, 'empty body, implicit invocant';
is-deeply D.new.c, Nil, 'empty body, typed explicit invocant only';
is-deeply D.new.d, Nil, 'empty body, named explicit invocant';
is D.new.c.raku, 'Nil', '.raku of the result is Nil';
