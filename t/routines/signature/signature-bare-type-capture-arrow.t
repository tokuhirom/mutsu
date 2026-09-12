use Test;

# A BARE type capture — `::Enum` with no variable of its own — ends where the
# next parameter or the signature's return constraint begins:
#
#     method add-enum-type(Str $name, ::Enum --> Promise) { ... }
#
# That is how Protocol::Postgres (and Net::Postgres, which depends on it)
# declares it. mutsu's capture branch listed `)`, `]`, `,` and `;` as the
# things that may follow a bare capture but not `-->`, so the arrow fell
# through to the "anything after the capture is a parameter in its own right"
# path, which then tried to parse `--> Promise)` as a parameter and failed.
# The arrow belongs to the enclosing signature, exactly like the closing paren.

plan 8;

# The distribution's own shape.
{
    class C {
        method add-enum-type(Str $name, ::Enum --> Promise) { Promise.kept($name) }
    }
    is C.add-enum-type('x', Int).result, 'x',
        'a bare type capture may be followed by a return constraint';
}

# The capture is still a capture: it binds the argument's type.
{
    sub captured(::T --> Str) { T.^name }
    is captured(Int), 'Int', 'the bare capture still binds the type it was given';
    is captured(Str), 'Str', 'and follows the argument on a second call';
}

# More than one, and in the middle of a list.
{
    sub two(::T, ::U --> Str) { T.^name ~ '/' ~ U.^name }
    is two(Int, Str), 'Int/Str', 'two bare captures before the arrow';

    sub middle(::T, Int $n --> Int) { $n }
    is middle(Str, 5), 5, 'a bare capture followed by an ordinary parameter';
}

# Every terminator that already worked must keep working — the change adds one
# spelling to the list, it does not replace it.
{
    sub no-arrow(Str $n, ::Enum) { $n }
    is no-arrow('x', Int), 'x', 'a bare capture at the end of the list still works';

    sub with-var(::T $x --> Int) { 1 }
    is with-var(5), 1, 'a capture that has its own variable still works with an arrow';

    sub ordinary(Int $a --> Int) { $a }
    is ordinary(3), 3, 'an ordinary parameter with a return constraint is unaffected';
}
