use Test;

plan 2;

class Foo {
    method bar(::(q<Foo>) $x) { 'bar called' }
}
is Foo.new.bar(Foo.new), 'bar called',
    'an indirect type constraint works in a class method';

role Bar {
    method reparent(::(q<Foo>) $parent) { 'reparent called' }
}
class Baz does Bar { }
is Baz.new.reparent(Foo.new), 'reparent called',
    'an indirect type constraint does not drop a role method';
