use Test;

# A `Mu`-typed parameter, return or variable accepts every value: an instance
# of a user class, a type object, `Nil`, a Junction (bound whole, not
# autothreaded) and a Proxy. AttrX::Mooish passes instances through `Mu
# $instance is raw` / `Mu \obj` parameters on every lazy-attribute access, so
# this check is answered before the per-class instance memo.

plan 12;

class Foo { }
class Bar is Mu { }

sub plain(Mu $x) { $x.^name }
sub raw(Mu $x is raw) { $x.^name }
sub sigilless(Mu \x) { x.^name }
sub returns-mu(Mu $x --> Mu) { $x }

is plain(Foo.new), 'Foo', 'Mu $x takes an instance';
is plain(Foo), 'Foo', 'Mu $x takes a type object';
is plain(Bar.new), 'Bar', 'Mu $x takes an instance of a class that is not Any';
is plain(Nil), 'Nil', 'Mu $x takes Nil';
is plain(1|2), 'Junction', 'Mu $x takes a Junction without autothreading';
is raw(Foo.new), 'Foo', 'Mu $x is raw takes an instance';
is sigilless(Foo.new), 'Foo', 'Mu \x takes an instance';
is returns-mu(Bar.new).^name, 'Bar', '--> Mu returns an instance of a non-Any class';

my Mu $m = Bar.new;
is $m.^name, 'Bar', 'a Mu variable holds an instance of a non-Any class';

my $p := Proxy.new(FETCH => -> $ { Foo.new }, STORE => -> $, $ { });
is plain($p), 'Foo', 'Mu $x takes the value a Proxy fetches';

class M {
    method take(Mu $instance is raw, Mu :$type is raw = Foo) { $instance.^name ~ '/' ~ $type.^name }
}
is M.new.take(Bar.new), 'Bar/Foo', 'a method takes an instance through a Mu raw parameter';

sub any-only(Any $x) { 'any' }
throws-like { any-only(Bar.new) }, X::TypeCheck::Binding,
    'an Any parameter still rejects an instance of a class that is not Any';
