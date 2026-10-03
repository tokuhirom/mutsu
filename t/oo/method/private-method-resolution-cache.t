use Test;

# `$obj!name(...)` resolves through a type-keyed cache (#9494). These pin that
# the cache never lets a call skip the binding check the uncached path makes:
# an argument of another type, or a value a `where` clause rejects, still
# fails after a successful call has populated the cache.

plan 9;

class Typed {
    method !take(Int $x) { "Int:$x" }
    method go($v) { self!take($v) }
}
my $t = Typed.new;
is $t.go(1), 'Int:1', 'a typed private method accepts an Int';
dies-ok { $t.go('a') }, 'a Str argument still fails the Int parameter';
is $t.go(2), 'Int:2', 'and an Int is accepted again afterwards';

class Where {
    method !neg(Int $x where * < 0) { "neg:$x" }
    method go($v) { self!neg($v) }
}
my $w = Where.new;
is $w.go(-3), 'neg:-3', 'a where-constrained private method accepts a match';
dies-ok { $w.go(3) }, 'the same type with a rejected value still fails';
is $w.go(-1), 'neg:-1', 'and a matching value is accepted again';

class Single {
    has $.seen = 0;
    method !bump(Int $by) { $!seen += $by; self }
    method go(Int $n) { self!bump($n) for ^3; $!seen }
}
is Single.new.go(2), 6, 'a private method with an argument runs on every call';

class Base {
    method !who { 'base' }
    method ask { self!who }
}
class Derived is Base { }
is Derived.new.ask, 'base', 'a private method is found from a subclass instance';

class Outer {
    has @.log;
    method !note(Str $s) { @!log.push($s) }
    method run { self!note($_) for <a b c>; @!log.join(',') }
}
is Outer.new.run, 'a,b,c', 'a private method mutating an attribute array';
