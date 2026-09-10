use Test;

plan 3;

class Foo {
    has $.x is rw;
    method boo { $.x }
}

class Bar is Foo {
    method set($v) { $.x = $v }
}

my Foo $u .= new(x => 5);
is($u.boo, 5, 'set attribute');

{
    $u = Bar.new(Foo{ x => 12 });
    is($u.boo, 12, 'set parent attribute');
    $u.set(9);
    is($u.boo, 9, 'reset parent attribute');
}
