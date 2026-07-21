use Test;

# `also is <parent>` inside a class body must accept a *qualified* parent name.
# The `also is` parser read a bare identifier, so it stopped at the first `::` —
# `also is X::Config::TOML::DuplicateKeys` took just `X` as the parent and left
# `::…` as a stray term, giving "cannot inherit from 'X' because it is unknown"
# (Config::TOML, Crane, and other exception hierarchies).

plan 5;

{
    class X::Base { }
    class X::Derived { also is X::Base; }
    ok X::Derived.new ~~ X::Base, 'also is a qualified X::-rooted parent inherits';
}

{
    class Foo::Bar::Base { }
    class Foo::Bar::Deep { also is Foo::Bar::Base; }
    ok Foo::Bar::Deep.new ~~ Foo::Bar::Base, 'also is a deep qualified parent inherits';
}

# No regression: a simple (unqualified) parent still works.
{
    class Plain { }
    class Sub { also is Plain; }
    ok Sub.new ~~ Plain, 'also is an unqualified parent still works';
}

# No regression: `also is rw` is a class trait, not a parent.
{
    class C { has $.x is rw; also is rw; has $.y }
    my $o = C.new;
    lives-ok { $o.x = 1; $o.y = 2 }, 'also is rw still applies the rw class trait';
}

# The inheritance is real: methods are inherited through the qualified parent.
{
    class X::WithMethod { method greet { 'hi' } }
    class X::Child { also is X::WithMethod; }
    is X::Child.new.greet, 'hi', 'a method is inherited through the qualified parent';
}
