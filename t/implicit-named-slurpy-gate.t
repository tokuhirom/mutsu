use Test;

# Every method carries an implicit `*%_` holding the named arguments no explicit
# named parameter consumed. The compiled fast path only *materializes* it when
# the method body can observe it (`CompiledCode::may_observe_named_slurpy`),
# which is decided conservatively at compile time -- so cover each way a body
# reaches `%_`: directly, through an explicit `*%_` forwarded with `|%_`, from a
# nested closure, and across a `nextsame` deferral.

plan 9;

class A {
    has $.x;
    method plain($v) { return $v + 1 }
    method direct(:$a) { return %_.keys.sort.join(",") }
    method forwards(*%_) { return self.bless(|%_) }
    method in-closure() { my $c = { %_<k> // "none" }; return $c() }
    method empty-slurpy() { return %_.elems }
    method evals() { return EVAL '40 + 2' }
}

my $a = A.new(x => 1);

is $a.plain(1), 2, 'a method that never names %_ still binds its parameters';
is $a.direct(:a(1), :b(2), :c(3)), 'b,c',
    '%_ holds exactly the named args no explicit parameter consumed';
is A.forwards(x => 9).x, 9, 'an explicit *%_ forwards through |%_ to bless';
is $a.in-closure(k => 'hi'), 'hi', 'a nested closure sees the method %_';
is $a.empty-slurpy(), 0, '%_ is an empty Hash when no named args are left over';
is $a.direct(), '', '%_ is empty, not Any, when the call passes no named args';
is $a.evals(), 42, 'an EVAL in a method body still runs';

# A deferral re-dispatches the original arguments, so the next candidate must
# see its own `%_` even though the first candidate never named it.
class Base {
    method who() { return "base:" ~ %_.keys.sort.join(",") }
}
class Derived is Base {
    method who() { nextsame }
}
is Derived.new.who(:p(1), :q(2)), 'base:p,q',
    'a deferral hands the next candidate its own %_';

# `bless` through a user `new` that never names `%_` must still initialise
# attributes from the named arguments it forwards positionally.
class Point {
    has $.x;
    has $.y;
    submethod TWEAK(:$!x, :$!y) { }
}
is Point.new(x => 3, y => 4).x + Point.new(x => 3, y => 4).y, 7,
    'attributive named parameters bind without materialising %_';
