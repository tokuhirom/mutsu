use Test;
plan 13;

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

# A subclass's private scalar beside the parent's public code attribute of the
# same name (Test::Describe::Match::Change): a closure reads its own scalar.
class PubCode { has &.msg = -> $a { "parent $a" } }
class PrivScalar is PubCode {
    has $!msg = "child msg";
    method msg { -> $ { $!msg } }
    method set { $!msg = "changed" }
}
my $ps = PrivScalar.new;
is $ps.msg.(1), 'child msg', 'a closure reads the private scalar, not the parent code attribute';
$ps.set;
is $ps.msg.(1), 'changed', 'and sees the assignment';
