use Test;

# A `method` declared inside a routine body of a class is still installed in
# the class, closing over the routine's latest invocation; a `proto method`
# used as a term is that proto (#10804).

plan 13;

class Foo {
    method mk($n) {
        multi method handler(Foo:U: $x) { "U $x $n" }
        multi method handler(Foo:D: $x) { "D $x $n" }
        method plain { "plain $n" }
        42
    }
    sub helper($n) { method from-sub { "sub $n" } }
    method run-helper($n) { helper($n) }
}

is Foo.^lookup('handler').candidates.elems, 2, 'the multi candidates are installed';
is Foo.mk(7), 42, 'the routine itself runs';
is Foo.handler(3), 'U 3 7', 'a type-object call picks the :U candidate';
is Foo.new.handler(4), 'D 4 7', 'an instance call picks the :D candidate';
is Foo.plain, 'plain 7', 'a plain method closes over the call';
Foo.mk(8);
is Foo.plain, 'plain 8', 'and over the latest call';
Foo.run-helper(5);
is Foo.from-sub, 'sub 5', 'a method declared in a sub body is installed too';

class Bar {
    method mk() {
        my constant &p = proto method handler(|) {*}
        multi method handler(Bar:U: $x) { "U $x" }
        multi method handler(Bar:D: $x) { "D $x" }
        &p
    }
}
my $p = Bar.mk;
isa-ok $p, Method, 'a proto method term is a Method';
is $p.name, 'handler', 'named after the declaration';
ok $p.is_dispatcher, 'the dispatcher';
is $p.candidates.elems, 2, 'carrying the candidates declared beside it';
is $p(Bar, 1), 'U 1', 'invoking it dispatches a type object';
is $p(Bar.new, 2), 'D 2', 'and an instance';
