use Test;

plan 22;

# Rakudo words a failed store as `... expected R but got F (F.new)`: the offending
# OBJECT's class name, then its `.raku` in parentheses. mutsu named every object
# `Any` and printed no repr (a defined non-Fetcher object read as `got Any` in the
# zef plugin loader), because the pure message builders could not call `.raku`.

role R { }
class F { }
class G { has $.x = 1; has $!hidden = 2 }
class H { method raku { "CUSTOM" } }

# The reported repro.
class C { has R $.r }
try C.new(:r(F.new));
is $!.message, 'Type check failed in assignment to $!r; expected R but got F (F.new)',
    'an attribute initialised with an object of the wrong class names the class and its repr';
isa-ok $!, X::TypeCheck::Assignment, 'and is a typed X::TypeCheck::Assignment';
isa-ok $!.got, F, '.got is the offending object itself';
is $!.got.^name, 'F', 'whose class is the one the message names';

# The default `.raku` lists the public attributes only.
try C.new(:r(G.new));
is $!.message, 'Type check failed in assignment to $!r; expected R but got G (G.new(x => 1))',
    'the repr is the default `.raku`: public attributes, no private ones';

# A user-declared `raku` is what the message shows, so it runs through method dispatch.
try C.new(:r(H.new));
is $!.message, 'Type check failed in assignment to $!r; expected R but got H (CUSTOM)',
    'a class that declares `raku` is rendered by it';

# A long repr is cut to 20 characters plus an ellipsis, objects and strings alike.
class Wide { has $.text = "b" x 40 }
try C.new(:r(Wide.new));
is $!.message,
    'Type check failed in assignment to $!r; expected R but got Wide (Wide.new(text => "bb...)',
    'a long object repr is truncated';
try C.new(:r("a" x 22));
is $!.message,
    'Type check failed in assignment to $!r; expected R but got Str ("aaaaaaaaaaaaaaaaaaa...)',
    'a long string repr is truncated the same way';
try C.new(:r("a" x 21));
is $!.message,
    'Type check failed in assignment to $!r; expected R but got Str ("aaaaaaaaaaaaaaaaaaaaa")',
    'a repr of exactly 23 characters is kept whole';

# Every route to the same failure agrees.
class D { has Int $.n is rw; has Int $!p; has Int @.items; has Int %.map;
    method set-private($v) { $!p = $v }
    method set-via-self($v) { self.n = $v }
    method push-item($v) { @!items.push($v) }
    method store-key($v) { %!map<k> = $v }
}
my $d = D.new(n => 1);
try $d.n = G.new;
is $!.message, 'Type check failed in assignment to $!n; expected Int but got G (G.new(x => 1))',
    'an assignment through the `is rw` accessor';
try $d.set-private(G.new);
is $!.message, 'Type check failed in assignment to $!p; expected Int but got G (G.new(x => 1))',
    'an assignment to a private attribute inside a method';
try $d.set-via-self(G.new);
is $!.message, 'Type check failed in assignment to $!n; expected Int but got G (G.new(x => 1))',
    'an assignment through `self.n`';
try $d.push-item(G.new);
is $!.message, 'Type check failed for an element of @!items; expected Int but got G (G.new(x => 1))',
    'an element pushed onto a typed array attribute';
try $d.store-key(G.new);
is $!.message, 'Type check failed for an element of %!map; expected Int but got G (G.new(x => 1))',
    'an element stored in a typed hash attribute';

# Plain variables share the builders.
my Int $x;
try $x = G.new;
is $!.message, 'Type check failed in assignment to $x; expected Int but got G (G.new(x => 1))',
    'a typed scalar variable';
is $!.expected.^name, 'Int', '.expected is the expected type object';
my Int @a;
try @a[0] = G.new;
is $!.message, 'Type check failed for an element of @a; expected Int but got G (G.new(x => 1))',
    'a typed array element';
try { my Int $y := G.new };
is $!.message, 'Type check failed in binding; expected Int but got G (G.new(x => 1))',
    'a typed `:=` binding';

# Parameter binding of a method, a pointy block and a `for` loop parameter.
class P { method take(Int $i) { } }
try P.new.take(G.new);
is $!.message, "Type check failed in binding to parameter '\$i'; expected Int but got G (G.new(x => 1))",
    'a method parameter';
my &blk = -> Int $i { };
try blk(G.new);
is $!.message, "Type check failed in binding to parameter '\$i'; expected Int but got G (G.new(x => 1))",
    'a pointy-block parameter';
try { for G.new -> Int $v { } }
is $!.message, "Type check failed in binding to parameter '\$v'; expected Int but got G (G.new(x => 1))",
    'a `for` loop parameter';

# A class nested in another is named by its qualified name.
class Outer { class Inner { }; has Int $.n; method m { $!n = Inner.new } }
try Outer.new.m;
is $!.message,
    'Type check failed in assignment to $!n; expected Int but got Outer::Inner (Outer::Inner.new)',
    'a nested class is named qualified';
