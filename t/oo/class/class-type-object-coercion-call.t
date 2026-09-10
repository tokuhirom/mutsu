use v6;
use Test;

# Invoking a type object coerces: `Foo($x)` is `Foo.COERCE($x)` when the type
# defines COERCE, else `Foo.new($x)`, else X::Coerce::Impossible. Built-in types
# (`Int("42")`), roles and enums already took that path in mutsu; a
# user-declared *class* had none, so `Locale::Dates($locale)` died with
# "Unknown function: Dates" — the qualified name collapsed to its last component
# and was looked up as a routine.

plan 15;

{
    my class B { method new($x) { self.bless } }
    isa-ok B("q"), B, 'a class with `new` coerces through it';
}

{
    my $seen;
    my class B { method COERCE($x) { $seen = $x; self.bless } }
    isa-ok B("q"), B, 'a class with COERCE coerces through it';
    is $seen, 'q', 'and receives the argument';
}

{
    my @order;
    my class B {
        method new($x)    { @order.push('new');    self.bless }
        method COERCE($x) { @order.push('coerce'); self.bless }
    }
    B("q");
    is-deeply @order, ['coerce'], 'COERCE wins over new';
}

{
    my class B { method CALL-ME($x) { "called:$x" } }
    is B("q"), 'called:q', 'CALL-ME wins over both';
}

{
    my class B { }
    throws-like { B("q") }, X::Coerce::Impossible,
        'a class with none of the three is an impossible coercion';
    throws-like { B("q") }, X::Coerce::Impossible,
        message => /'Impossible coercion from' .* "'B'"/,
        'and says so the way raku does';
}

# A qualified class name must not collapse to its last component.
{
    my class Foo::Bar { multi method new($x = 'd') { self.bless } }
    is Foo::Bar("q").^name, 'Foo::Bar', 'a qualified class name coerces';
}

# A coercion takes ONE value: several arguments are coerced as a single List,
# they are not splatted. So a one-parameter `new` accepts `B("q", "r")` (it
# receives the List) and a two-parameter one does not.
{
    my class B { method new($x) { self.bless } }
    isa-ok B("q", "r"), B, 'several arguments coerce as a single List';

    my class C { method new($x, $y) { self.bless } }
    dies-ok { C("q", "r") },
        'and a two-parameter new does not match that single List';
}

# The type object itself is unchanged when called with no arguments.
{
    my class B { method new($x = 1) { self.bless } }
    ok B.defined.not, 'a bare type name is still the type object';
}

# A COERCE with no matching candidate falls back to `new`, as raku does.
{
    my class B {
        multi method COERCE(Str:D $s) { self.bless }
        multi method new(Int:D $n)    { self.bless }
    }
    isa-ok B("x"), B, 'COERCE handles the spelling it declares';
    isa-ok B(42),  B, 'and a non-matching COERCE falls back to new';
}

# Roles must keep their own path: coercing a role puns it to a class, so a
# class-first branch would shadow the role branch on every later call.
# (roast/S12-coercion/coercion-methods.t "Roles" caught exactly this.)
{
    my role R {
        has Str:D $.attr is required;
        multi method new(Int:D $n)    { self.new(attr => $n.Str) }
        multi method COERCE(Str:D $s) { R.new(attr => $s) }
    }
    is R("The Answer").attr, 'The Answer', 'a role coerces through COERCE';
    is R(42).attr, '42', 'and still reaches its `new` on a later call';
}
