use Test;

plan 9;

# A closure created inside a method is still lexically inside its class, so
# `self!priv` from its body stays legal however late the closure is invoked.

class C {
    method !secret(*@args) { 'S:' ~ @args.join(',') }
    method direct()      { self!secret(1, 2) }
    method via-sub()     { sub (*@args) { self!secret(|@args) } }
    method via-pointy()  { -> $x { self!secret($x) } }
    method via-var()     { my $b = { self!secret(9) }; $b }
    method via-map()     { (1, 2).map({ self!secret($_) }).join('|') }
    method via-nested()  { sub { sub { self!secret(7) } } }
    method via-deep()    { my $f = sub { self!secret(5) }; my $g = sub { $f() }; $g }
}

my $c = C.new;
is $c.direct, 'S:1,2', 'a private call directly in a method still works';
is $c.via-sub()(3, 4), 'S:3,4', 'and from an anonymous sub returned by a method';
is $c.via-pointy()(8), 'S:8', 'and from a pointy block';
is $c.via-var()(), 'S:9', 'and from a block stored in a lexical';
is $c.via-map(), 'S:1|S:2', 'and from a block passed to .map';
is $c.via-nested()()(), 'S:7', 'and from a sub nested in a sub';
is $c.via-deep()(), 'S:5', 'and through a second closure that calls the first';

# The permission itself is unchanged: an out-of-class caller still may not make
# an unqualified private call, whether or not a closure is involved. (raku
# rejects these while compiling, so they have to be checked as source text.)
throws-like 'class K1 { method !s() { 1 } }; my $k = K1.new; $k!s()',
    X::Method::Private::Unqualified,
    'an unqualified private call from outside the class is still rejected';
throws-like 'class K2 { method !s() { 1 } }; my $k = K2.new; sub { $k!s() }()',
    X::Method::Private::Unqualified,
    'and wrapping it in a closure does not launder it';
