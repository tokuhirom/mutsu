use v6;
use Test;

# Regression pin for the scoped/overlay env on the compiled-method fast path
# (call_compiled_method_fast). See docs/vm-dual-store.md (Slice 6). A method with
# no inner closures runs under a born-owned scoped overlay so its self/?CLASS/
# param/attr env writes never fork the caller env; the overlay is dropped (or
# merged overlay-only) on return. The `$.attr` accessor read is a pure read and
# must NOT collapse the overlay.

plan 16;

class Point {
    has $.x;
    has $.y;
    method sum() { $.x + $.y }
    method scale($f) { Point.new(x => $.x * $f, y => $.y * $f) }
    method bump() { $!x++; $!x }
}
my $p = Point.new(x => 3, y => 4);
is $p.sum(), 7, 'method reads two attributes';
is $p.scale(10).sum(), 70, 'method constructs + chains';
is $p.bump(), 4, 'method mutates private attr';
is $p.bump(), 5, 'mutation persists on the instance';
is $p.x, 5, 'public accessor reflects mutation';
is $p.sum(), 9, 'sum after mutation (5+4)';

# method mutating a captured outer global must persist
my $total = 0;
class Adder { has $.n; method add() { $total += $.n; } }
Adder.new(n => 5).add();
Adder.new(n => 7).add();
is $total, 12, 'method mutates captured outer global';

# nested method dispatch under scoped env
class Calc {
    has $.base;
    method double() { $.base * 2 }
    method quad() { self.double() * 2 }
}
is Calc.new(base => 3).quad(), 12, 'nested self.method() dispatch';

# method returning a closure (non-scoped path) still works
class Maker { has $.k; method make() { my $b = $.k; -> $x { $b + $x } } }
my &f = Maker.new(k => 100).make();
is f(5), 105, 'closure-returning method captures attr';
is f(10), 110, 'returned closure reusable';

# inheritance + SUPER-style redispatch
class Base { method greet() { "base" } }
class Derived is Base { method greet() { "derived-" ~ self.Base::greet() } }
is Derived.new.greet(), 'derived-base', 'inherited redispatch';

# method with a positional param + type constraint
class Box { has $.v; method add(Int $d) { $.v + $d } }
is Box.new(v => 10).add(5), 15, 'method with typed positional param';

# method calling a free sub (nested function call flattens scoped env)
sub helper-fn($x) { $x * 3 }
class User { has $.m; method use-it() { helper-fn($.m) } }
is User.new(m => 4).use-it(), 12, 'method calling a free sub';

# repeated calls on the same object are independent (no overlay leak)
class Counter { has $.start; method val() { my $t = $.start; $t + 1 } }
my $c = Counter.new(start => 41);
is $c.val(), 42, 'first call';
is $c.val(), 42, 'second call independent';

# method mutating a captured array
my @log;
class Logger { has $.tag; method log() { @log.push($.tag); } }
Logger.new(tag => 'a').log();
Logger.new(tag => 'b').log();
is @log.join(','), 'a,b', 'method mutates captured array';
