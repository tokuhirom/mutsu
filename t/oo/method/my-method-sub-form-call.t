use Test;

# A class body's `my method NAME` is callable in its sub form `NAME(self, ...)`
# from sibling methods. mutsu resolves it from the class-lexical key directly
# instead of climbing the whole function-resolution ladder (perf).
plan 10;

my class C {
    has $.n = 10;
    my method add(Int $x) { $!n + $x }
    my method twice(Int $x) { add(self, $x) * 2 }
    my method fact(Int $k) { $k <= 1 ?? 1 !! $k * fact(self, $k - 1) }
    method a($x) { add(self, $x) }
    method b($x) { twice(self, $x) }
    method c($k) { fact(self, $k) }
    method d($x) { add self, $x }
}
my $c = C.new;
is $c.a(5), 15, 'sub form reads an attribute of the invocant';
is $c.b(5), 30, 'a my method calling another my method';
is $c.c(5), 120, 'recursion through the sub form';
is $c.d(7), 17, 'list-operator call form';
is (C.new(n => 1).a(1)), 2, 'a second instance';
for ^3 { $c.a($_) }
is $c.a(1), 11, 'repeated calls stay correct';

# A lexical beats a same-named package routine.
our sub add(Int $x) { 'package' }
is $c.a(1), 11, 'the lexical my method wins over a package sub of that name';

# A nearer lexical &name shadows the class-level one.
my class D {
    my method st($t) { "method $t" }
    method viaparam(&st, $t) { st($t) }
    method plain($t) { st(self, $t) }
}
is D.new.viaparam(-> $t { "param $t" }, 1), 'param 1', 'a &st parameter shadows the my method';
is D.new.plain(2), 'method 2', 'without the parameter the my method is called';

# A sub form call with the wrong arity still fails like any call.
my class E {
    my method one(Int $x) { $x }
    method bad() { one(self) }
}
dies-ok { E.new.bad }, 'a missing argument is still an error';
