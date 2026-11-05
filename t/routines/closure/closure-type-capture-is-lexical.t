use Test;

# From the Deps ecosystem distribution: a closure created in a routine with a
# `::T` type capture must keep reading THAT frame's `T` when it is called from
# another routine that has its own `::T`. It used to read the caller's.

plan 8;

sub mk(::T) { -> { T.^name } }
sub call(::T, &f) { f() }

role R { }
class C2 does R { }

is call(Str, mk(Int)), 'Int', 'a pointy closure keeps its creator\'s T';
is call(Str, mk(C2)), 'C2', 'also for a user class';

sub mk-vars(::T) { my $v = 5; -> { "{T.^name} $v" } }
is call(Str, mk-vars(Int)), 'Int 5', 'together with a captured scalar';

sub mk-dispatch(::T) { sub { T.^name } }
is call(Str, mk-dispatch(Num)), 'Num', 'an inner sub as well';

class D {
    has @.items;
    method register(::Type) { @!items.push: -> { Type.^name } }
    method get(::Type) { @!items[0].() }
}
my $d = D.new;
$d.register(C2);
is $d.get(R), 'C2', 'a closure stored by a method, run from a method with its own ::Type';

# the original shape: a factory builds `Type.new`, called under another Type
class Container {
    has %.f;
    multi method register(Any:U ::Type) { %!f{Type.^name} = -> |c { Type.new } }
    method get(::Type, Str :$as) { %!f{$as}.() }
}
my $c = Container.new;
$c.register(C2);
is $c.get(R, :as<C2>).^name, 'C2', 'the factory builds the registered class, not the requested role';

is call(Str, -> { 'plain closure' }), 'plain closure', 'closures without a capture are unchanged';
sub reads-own(::T) { T.^name }
is reads-own(Int), 'Int', 'a routine reading its own T is unchanged';
