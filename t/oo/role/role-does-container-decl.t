# Found via the OrderedHash distribution: `my %h does Role[...]` on a container.
use Test;

plan 14;

role R[::T = Any, :@keys = <a b>] does Associative {
    has T @!values is default(T);
    has Str @!keys = @keys;
    has UInt %!map = @keys.kv.reverse;
    method STORE(*@pairs, :$initialize) {
        for @pairs -> (:$key, :$value) { self{$key} = $value }
        self
    }
    method !index(Str() \key where any @!keys --> UInt) { %!map{key} }
    method of { T }
    method k { @!keys }
    method keys { @!keys.grep: { self{$_}:exists } }
    method AT-KEY(Str() \key) is rw { @!values[self!index(key)] }
    method EXISTS-KEY(Str() \key) { @!values[self!index(key)].DEFINITE }
    method DELETE-KEY(Str() \key) { @!values[self!index(key)]:delete }
    method ASSIGN-KEY(Str() \key, \value) { @!values[self!index(key)] = value }
}

# a role-declared `of` wins over the native container `.of`
my %a does R;
ok %a.of === Any, 'default role parameter reaches a role method on a hash';
my %b does R[Str];
ok %b.of === Str, 'explicit role parameter reaches a role method on a hash';
my @c does R[Int];
ok @c.of === Int, '... and on an array';

# unset slots read as the typed attribute's element type
ok %b<a> === Str, 'typed role attribute: unset element is the type object';
ok (%b<a>:delete) === Str, 'typed role attribute: :delete of unset element';

# `my %h does R[...] = list` takes the initializer after mixing in the role
my %d does R[:keys<2 3 1>] = 1 => 3, 2 => 1, 3 => 2;
is %d.k, <2 3 1>, 'role named argument survives the initializer';
is %d.keys, <2 3 1>, 'STORE ran on the mixed-in hash';
is %d<2>, 1, 'initializer pairs were stored';

# a failed bind of a lone private method is a type-check error
throws-like { %b<zz> = 1 }, X::TypeCheck::Binding::Parameter,
    'private method where-constraint failure';
lives-ok { %b<a> = 'x'; %b<b> = 'y' }, 'valid keys still assign';
is %b.keys, <a b>, 'keys in declared order';
is %b<b>, 'y', 'value round-trips';

role Q { method foo { 'foo' } }
my @e does Q = <x y>;
is @e.foo, 'foo', 'array does role with initializer';
is @e.elems, 2, 'initializer applied';
