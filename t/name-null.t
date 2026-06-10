use Test;

# A qualified name with an empty component between `::` separators
# (`$a::::b`, `Foo::::Bar`) is a null name component — Raku rejects it at
# compile time with X::Syntax::Name::Null.

plan 6;

throws-like 'my $a::::b', X::Syntax::Name::Null, 'scalar with null component';
throws-like 'Foo::::Bar.new', X::Syntax::Name::Null, 'type name with null component';
throws-like '@a::::b', X::Syntax::Name::Null, 'array with null component';

# Ordinary qualified names are unaffected.
lives-ok {
    my $Foo::bar = 5;
    die unless $Foo::bar == 5;
    my $trailing::;     # package-stash variable, legal
}, 'normal `Foo::bar` and trailing `::` still work';

lives-ok { class A::B::C { method m { 42 } }; die unless A::B::C.m == 42; },
    'deeply qualified class name still works';

# Single `::` separators are fine.
is ('a' ~ '::' ~ 'b'), 'a::b', 'sanity';
