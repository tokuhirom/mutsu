use Test;

# A custom container trait (`my @a is Tied = ...`) hands STORE the initializer
# as written. The compiler leaves that raw value on the stack for the one
# `ApplyVarTrait` that consumes it (ADR-10779 D3), so it survives a package
# store in between, a STORE that declares its own traited container, and
# never leaks into a later declaration. Outputs checked against rakudo.

plan 9;

class Tied does Positional {
    has @.store;
    method STORE(\v, :$INITIALIZE) { @!store = v ~~ Positional ?? v.list !! (v,); self }
    method AT-POS($i) { @!store[$i] }
    method elems { @!store.elems }
    method raw-shape { @!store.raku }
}

class TiedH does Associative {
    has %.store;
    has $.got;
    method STORE(\v, :$INITIALIZE) { $!got = v.raku; %!store = v; self }
    method AT-KEY($k) { %!store{$k} }
}

class Outer does Positional {
    has @.inner;
    method STORE(\v, :$INITIALIZE) {
        my @tmp is Tied = 'inner';
        @!inner = @tmp.raw-shape, v.raku;
        self
    }
}

my @a is Tied = 'x';
is @a.raw-shape, '["x"]', 'a scalar initializer reaches STORE as a scalar';

my @b is Tied = 'x', 'y';
is @b.raw-shape, '["x", "y"]', 'a list initializer reaches STORE as a list';

our @c is Tied = 'p', 'q';
is @c.raw-shape, '["p", "q"]', 'an our declaration hands STORE the same initializer';

my %h is TiedH = a => 1;
is %h.got, ':a(1)', 'a hash custom container gets the Pair as written';

my $r = (my @d is Tied = 'm');
is @d.raw-shape, '["m"]', 'an expression-position declaration hands STORE its initializer';

my @e is Outer = 'outer';
is-deeply @e.inner, ['["inner"]', '"outer"'],
    'a STORE that declares its own traited container keeps both initializers apart';

my @f is Tied;
is @f.elems, 0, 'a later declaration without an initializer sees no stale initializer';

my @g = 1, 2;
my @l is List = 0, @g;
is @l[1].raku, '[1, 2]', 'is List keeps the initializer elements as written';

my %m is Map = (b => Nil, c => 3);
ok %m<b> =:= Nil, 'is Map reads Nil values from the initializer as written';
