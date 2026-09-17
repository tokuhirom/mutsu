use v6;
use Test;

# `Foo($x)` on a type object that declares `CALL-ME` must dispatch to it,
# never to the "calling a type with a type-object argument constructs a
# coercion type object" shortcut (`Str(Any)`, `Int(Str)`, ...). That shortcut
# used to fire whenever the single argument was itself undefined (an
# uninitialized `my $y;`, whose container holds `Any`/`Nil`), preempting
# `CALL-ME` and silently answering the symbolic type object `Foo(Any)`
# instead of running the user's method. Found via the `Trap` ecosystem
# distribution's `Trap(my $*OUT)` idiom (`is raw` parameter aliasing a
# freshly-declared, still-undefined dynamic variable).

plan 5;

{
    my class Foo {
        method CALL-ME(Foo:U: $one is raw) {
            $one = 42;
        }
    }
    my $y;
    my $out = Foo($y);
    is $out, 42, 'CALL-ME wins when the argument is an already-existing undefined var';
    is $y, 42, 'and the is-raw parameter still writes back through it';
}

{
    # Same shape, but the declaration and the call sit inside a nested block
    # -- the exact syntactic form `Trap(my $*OUT)` uses (a fresh declaration
    # written inline as the call's argument).
    my class Foo {
        method CALL-ME(Foo:U: $one is raw) {
            $one = 42;
        }
    }
    my $out;
    {
        $out = Foo(my $y);
        is $y, 42, 'and the is-raw parameter writes back through an inline-declared var';
    }
    is $out, 42, 'CALL-ME wins when the argument is declared inline in the call';
}

{
    # A type with NO `CALL-ME` still gets the coercion-type-term answer for
    # an undefined argument -- this branch is not being removed, only gated.
    my class Bar { }
    is Bar(my $z).raku, 'Bar(Any)',
        'a type with no CALL-ME still builds the coercion type term for an undefined arg';
}
