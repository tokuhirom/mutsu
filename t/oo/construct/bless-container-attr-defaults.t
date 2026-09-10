use v6;
use Test;

plan 12;

# A custom `new` that routes through `self.bless(...)` must still default
# unassigned `@!`/`%!` attributes to an empty Array/Hash, not leave them Nil.
# Leaving an array attribute as Nil makes `@!attr.elems` return 1 (Any.elems),
# which silently corrupts any guard like `return @!cache if @!cache.elems`.

# --- direct bless ---
{
    class A { has @!c; method probe { @!c } }
    my $a = A.bless;
    is $a.probe.^name, 'Array', 'direct bless: @! attr defaults to Array';
    is $a.probe.elems, 0, 'direct bless: @! attr is empty';
}

{
    class B { has %!h; method probe { %!h } }
    my $b = B.bless;
    is $b.probe.^name, 'Hash', 'direct bless: %! attr defaults to Hash';
    is $b.probe.elems, 0, 'direct bless: %! attr is empty';
}

{
    class C { has @.pub; }
    my $c = C.bless;
    is $c.pub.^name, 'Array', 'direct bless: @. public attr defaults to Array';
    is $c.pub.elems, 0, 'direct bless: @. public attr is empty';
}

# --- typed array attribute keeps its element type ---
{
    class D { has Int @!c; method probe { @!c } }
    my $d = D.bless;
    is $d.probe.^name, 'Array[Int]', 'direct bless: typed @! attr keeps element type';
    is $d.probe.elems, 0, 'direct bless: typed @! attr is empty';
}

# --- custom new -> bless with extra named arg (Zef::Distribution pattern) ---
{
    class E {
        has $.name;
        has @!cache;
        method new(*%_) { self.bless(|%_, :meta(%_)) }
        method lazy-list(--> Array) {
            return @!cache if @!cache.elems;   # guard must not fire on empty
            my @r = ();
            return @!cache := @r;
        }
    }
    my $e = E.new(:name<x>);
    is $e.name, 'x', 'custom new: named attr set';
    my $ll := $e.lazy-list;
    is $ll.^name, 'Array', 'custom new: guarded lazy list returns Array';
    is $ll.elems, 0, 'custom new: guarded lazy list is empty';
}

# --- scalar attr with no default is still Any ---
{
    class F { has $!s; method probe { $!s } }
    is F.bless.probe.^name, 'Any', 'direct bless: $! attr with no default stays Any';
}
