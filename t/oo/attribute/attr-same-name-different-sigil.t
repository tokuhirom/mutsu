use Test;
plan 11;

class A {
    has $!f = "s";
    has &!f = -> $x { "c$x" };
    method both { "{$!f}|{&!f(4)}" }
    method call { (&!f)(1) }
    method call2 { &!f.(2) }
    method cl { -> { $!f } }
    method cc { -> { &!f } }
    method set { &!f = -> $x { "new$x" }; $!f = "s2"; (&!f)(3) ~ $!f }
}

is A.new.both, 's|c4', '$!f and &!f are distinct attributes';
is A.new.call, 'c1', '(&!f)() reads the code attribute';
is A.new.call2, 'c2', '&!f.() reads the code attribute';
is A.new.cl.(), 's', 'a closure reads $!f';
is A.new.cc.()(5), 'c5', 'a closure reads &!f';
is A.new.set, 'new3s2', 'assigning &!f leaves $!f alone';

class P { has &!g = -> { "p" }; method pg { &!g() } }
class K is P {
    has $!g = "k";
    method kg { $!g }
    method kc { -> { $!g } }
}
my $k = K.new;
is $k.pg, 'p', 'parent code attribute';
is $k.kg, 'k', 'child scalar attribute of the same name';
is $k.kc.(), 'k', 'a closure reads the child scalar';

class B { has @!f = 1, 2; has %!f = a => 1; has &!f = { 7 };
    method m { "{@!f}/{%!f}/{&!f()}" } }
is B.new.m, "1 2/a\t1/7", 'array, hash and code attributes of one name';
is A.^attributes.map(*.name).join(','), '$!f,&!f', '.^attributes lists both';
