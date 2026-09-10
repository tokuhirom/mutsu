use Test;

plan 4;

class Foo {
    our &m1 = method m1($a) { 1 }
    our &m2 = method m2($a, *%foo) { %foo.keys.elems }
}

lives-ok { Foo.new.m1(1, :x<1>, :y<2>) }, 'implicit *%_ accepts extra nameds';
ok &Foo::m1.signature.raku ~~ /'*%_'/, 'implicit *%_ appears in signature';
lives-ok { Foo.new.m2(1, :x<1>, :y<2>) }, 'explicit *%_ accepts extra nameds';
ok &Foo::m2.signature.raku !~~ /'*%_'/, 'explicit *%_ suppresses implicit auto-%_';
