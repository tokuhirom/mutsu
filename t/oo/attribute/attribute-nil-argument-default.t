use Test;

plan 12;

# Passing Nil for an attribute stores that attribute's container default, the
# same as assigning Nil: the `is default(...)` value, else the declared type
# object, else Any. The `= ...` initializer is NOT that default (it only runs
# when the attribute is not supplied at all).
class C {
    has $.m;
    has $.d = 5;
    has Int $.t;
    has $.i is default(42);
    has @.a;
}
is C.new(:m(Nil)).m.raku, 'Any', 'untyped attribute becomes Any';
is C.new(:d(Nil)).d.raku, 'Any', 'the initializer is not the Nil default';
is C.new(:t(Nil)).t.raku, 'Int', 'typed attribute becomes its type object';
is C.new(:i(Nil)).i, 42, 'is default(...) supplies the Nil default';
is C.new(:a(Nil)).a.raku, '[Any]', 'an @-attribute keeps its own Nil decay';

class P { has Int $.x }
class Q is P { }
is Q.new(:x(Nil)).x.raku, 'Int', 'an inherited attribute resolves its own type';

class R {
    has $.x;
    method new(*%a) { self.bless(|%a) }
}
is R.new(:x(Nil)).x.raku, 'Any', 'the same through bless';

class B {
    has $.x;
    submethod BUILD(:$!x) { }
}
is B.new(:x(Nil)).x.raku, 'Any', 'an attributive BUILD parameter binds by assignment';

my $seen;
class T {
    has $.x;
    submethod TWEAK { $seen = $!x.raku }
}
T.new(:x(Nil));
is $seen, 'Any', 'TWEAK already sees the default';

class D { has Str:D $.x = 'a' }
throws-like { D.new(:x(Nil)) }, X::TypeCheck::Assignment,
    'a :D attribute with no default rejects Nil';

# Assignment keeps working as before.
class W { has Int $.x is rw }
my $w = W.new(:x(3));
$w.x = Nil;
is $w.x.raku, 'Int', 'assigning Nil through an rw accessor';

# A coercion's fallback `new` sees `$*COERCION-TYPE` (roast
# S12-coercion/coercion-methods.t stores it in a `Mu`-typed attribute).
class Src { has $.value }
class Tgt {
    has Mu $.coercion-type;
    multi method new(::?CLASS:U: Src:D $s) { self.new: :coercion-type($*COERCION-TYPE) }
}
my Tgt(Any) $coerced = Src.new(:value<v>);
is $coerced.coercion-type.raku, 'Tgt(Any)', '$*COERCION-TYPE is the coercion type';
