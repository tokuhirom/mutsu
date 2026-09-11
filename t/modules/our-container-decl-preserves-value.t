use Test;

# A bare `our @a;` / `our %h;` declaration must LOAD the package variable, not
# reset it to the parser's synthesized empty container.
#
# The visible symptom was a `BEGIN` phaser: its body runs before the
# declaration statement that precedes it in the source, so
#
#     our %Store; BEGIN %Store = (a => 1); say %Store<a>;
#
# wrote a populated hash that the declaration then silently overwrote with an
# empty one — mutsu answered `(Any)` where rakudo answers `1`. The scalar case
# already worked (a bare `our $x;` compiled to `GetOurVar`); only the `%`/`@`
# halves took the reset path, because their synthesized default is an empty
# `Hash`/`Array` literal rather than `Nil`. The same reset also made a bare
# `our` container inside a sub start empty on every call. See #7953.

plan 17;

# --- the reported repro: a BEGIN write survives the declaration -------------
{
    our %Store;
    BEGIN %Store = (a => 1);
    is %Store<a>, 1, 'BEGIN write to an `our` hash survives the declaration';
}

{
    our %Block;
    BEGIN { %Block = (a => 2) };
    is %Block<a>, 2, 'block-bodied BEGIN write to an `our` hash survives';
}

{
    our @List;
    BEGIN @List = (1, 2);
    is @List.join(','), '1,2', 'BEGIN write to an `our` array survives';
}

# The scalar case, which always worked — pinned so it cannot regress.
{
    our $Scalar;
    BEGIN $Scalar = 5;
    is $Scalar, 5, 'BEGIN write to an `our` scalar survives';
}

# --- a fresh declaration still installs an empty container of the right type -
{
    our %Fresh;
    is %Fresh.^name, 'Hash', 'a never-written `our %` declares a Hash';
    is %Fresh.elems, 0, '...and it is empty';

    our @Fresh;
    is @Fresh.^name, 'Array', 'a never-written `our @` declares an Array';
    is @Fresh.elems, 0, '...and it is empty';
}

# --- an explicit initializer still runs, including an empty one -------------
{
    our %Init = (a => 1);
    is %Init<a>, 1, '`our %h = (...)` assigns';
    our %Init2 = (a => 1);
    %Init2 = ();
    is %Init2.elems, 0, 'a later explicit empty assignment still clears';

    our @Init = (1, 2);
    is @Init.join(','), '1,2', '`our @a = (...)` assigns';
}

# --- a bare redeclaration preserves what is already there -------------------
{
    our %Re = (a => 1);
    our %Re; #OK
    is %Re<a>, 1, 'bare `our %` redeclaration preserves pairs';

    our @Re = (1, 2);
    our @Re; #OK
    is @Re.join(','), '1,2', 'bare `our @` redeclaration preserves elements';
}

# --- a bare `our` container in a sub body accumulates across calls ----------
{
    sub bump() { our %Count; %Count<n>++ }
    bump; bump;
    our %Count;
    is %Count<n>, 2, 'a bare `our %` in a sub body accumulates across calls';
}

# --- expression position takes the same path --------------------------------
{
    our %Expr;
    BEGIN %Expr = (a => 3);
    my $h = (our %Expr); #OK
    is $h<a>, 3, 'expression-position bare `our %` loads the package variable';

    my $fresh = (our %ExprFresh);
    is $fresh.^name, 'Hash', 'a never-written expression-position `our %` is a Hash';
}

# --- a package-scoped declaration reaches its qualified name ----------------
{
    module M7953 {
        our @T;
        BEGIN @T = (7, 8);
    }
    is @M7953::T.join(','), '7,8', 'a module scope keeps its `our @` BEGIN-time value';
}
