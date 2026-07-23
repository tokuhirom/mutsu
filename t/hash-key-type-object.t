use v6;
use Test;

# A bare type object used as a plain (Str-keyed) hash subscript key coerces to
# the empty string with Rakudo's "uninitialized value of type X in string
# context" warning. This holds across store, read, :exists and :delete, so all
# four agree on the "" key. A user .Str/.Stringy dispatches instead (no warning),
# and an object hash (typed keys) keeps the type object.

plan 13;

# --- store keys "" ---
{
    my %h;
    quietly %h{Int} = 1;
    is %h.keys.raku, '("",).Seq', 'type object store keys ""';
    is %h.elems, 1, 'one entry stored';
}

# --- read finds the "" key ---
{
    my %h;
    quietly %h{Int} = 1;
    is (quietly %h{Int}), 1, 'read via type object finds "" entry';
    is (quietly %h{""}), 1, 'read via literal "" finds the same entry';
}

# --- distinct type objects collapse to the same "" key ---
{
    my %h;
    quietly %h{Int} = 1;
    quietly %h{Str} = 2;
    is %h.elems, 1, 'two distinct type objects collapse to one "" key';
    is (quietly %h{Int}), 2, 'the later write wins';
}

# --- :exists agrees with the store ---
{
    my %h;
    quietly %h{Int} = 1;
    ok (quietly %h{Int}:exists), 'type object :exists is True after store';
    ok (quietly %h{""}:exists), 'literal "" :exists is True too';
    my %g;
    nok (quietly %g{Int}:exists), 'type object :exists is False on an empty hash';
}

# --- :delete removes the "" key ---
{
    my %h;
    quietly %h{Int} = 1;
    quietly %h{Int}:delete;
    is %h.elems, 0, 'type object :delete removes the "" entry';
}

# --- a user .Str dispatches (no coercion to "") ---
{
    my class Foo { method Str { "foo" } }
    my %h;
    %h{Foo} = 1;
    is %h.keys.raku, '("foo",).Seq', 'user .Str type object keys by its .Str';
    ok %h{Foo}:exists, 'user .Str :exists agrees with the store';
}

# --- an object hash keeps the type object as a key ---
{
    my %h{Any};
    %h{Int} = 1;
    is %h.keys.raku, '(Int,).Seq', 'object hash keeps the type object key';
}

done-testing;
