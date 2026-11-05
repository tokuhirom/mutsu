use v6;
use Test;

# An element landing in an array is a container: a Proxy among the initializer
# of a typed array declaration is FETCHed before the element type check.
# Seen in POSIX::PWDENT, whose `do while $!_mem[$c] { $!_mem[$c++] }` over a
# NativeCall CArray yields Proxies that `has Str @.members` receives.

plan 5;

my $v = 'x';
my $p := Proxy.new(FETCH => -> $ { $v }, STORE => -> $, $n { $v = $n });

my Str @a = ($p,);
is @a.raku, 'Array[Str].new("x")', 'list initializer';
my Str @b = ($p,).Seq;
is @b.raku, 'Array[Str].new("x")', 'Seq initializer';
my Str @c = $p, $p;
is @c.elems, 2, 'comma list of Proxies';

class H { has Str @.m }
my @items = $p, $p;
is H.new(m => @items).m.elems, 2, 'a typed array attribute receives Proxies';

throws-like { my Int @d = ($p,) }, X::TypeCheck, 'the FETCHed value is still type checked';
