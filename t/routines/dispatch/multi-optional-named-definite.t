use Test;

plan 12;

# An OPTIONAL named parameter with a `:D` (definite) smiley and no default binds
# its type object (`:U`) when omitted, so it must NOT match a call that omits it.
# Otherwise `f(Int:D :$size)` wrongly out-dispatches the empty `f()`.
# (Algorithm::BinaryIndexedTree: `multi submethod BUILD(Int:D :$!size)` vs
# `multi submethod BUILD()` for a no-arg `.new`.)

{
    multi a() { "empty" }
    multi a(Int:D :$size) { "sized" }
    is a(),            "empty", ':D named candidate does not match a no-arg call';
    is a(size => 3),   "sized", ':D named candidate matches when supplied';
}

# An untyped or non-:D named param still binds its (undefined) type object when
# omitted, so it DOES match — and rakudo prefers the explicit-named candidate.
{
    multi b() { "empty" }
    multi b(:$x) { "named" }
    is b(),        "named", 'untyped optional named still matches (explicit-named wins)';

    multi c() { "empty" }
    multi c(Int :$size) { "sized" }
    is c(),        "sized", 'non-:D typed named still matches an omitting call';
    is c(size=>9), "sized", 'non-:D typed named matches when supplied';
}

# multi submethod BUILD dispatch: no-arg `.new` picks BUILD(), `.new(:size)`
# picks BUILD(Int:D :$!size) and the attribute-binding `:$!size` receives it.
{
    my class C {
        has @!tree;
        has Int $!size;
        multi submethod BUILD(Int:D :$!size) { for 0..$!size { @!tree.push(0) } }
        multi submethod BUILD() { $!size = 5; for 0..$!size { @!tree.push(0) } }
        method size  { $!size }
        method elems { @!tree.elems }
    }
    is C.new.size,             5, 'no-arg new -> BUILD() sets size=5';
    is C.new.elems,            6, 'no-arg new -> BUILD() built 6 slots';
    is C.new(size => 3).size,  3, 'new(:size) -> BUILD(:$!size) receives 3';
    is C.new(size => 3).elems, 4, 'new(:size) -> BUILD(:$!size) built 4 slots';
}

# `:$!attr` attribute-binding named param binds a matching `attr => ...` call arg
# (the `!` twigil is not part of the external named key).
{
    my class P {
        has Int $.n;
        submethod BUILD(Int:D :$!n) { }
        method get { $!n }
    }
    is P.new(n => 42).get, 42, ':$!n attribute-binding named binds n => 42';
}

# Required named (`:$x!`) is unaffected: it must be supplied.
{
    multi r() { "empty" }
    multi r(Int:D :$x!) { "req" }
    is r(),       "empty", 'required named candidate does not match an omitting call';
    is r(x => 1), "req",   'required named candidate matches when supplied';
}
