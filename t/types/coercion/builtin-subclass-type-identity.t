use lib $?FILE.IO.parent.add('lib').Str;
use Test;
use RoundedMod;

# An `is Array` subclass keeps its OWN type identity, and a `constant` alias
# for a class names that class wherever the class name would.
#
# Two independent gaps, both reached by the same packaging idiom
# (`class Array::Rounded is Array is export {}` + `my constant Rounded is export
# = Array::Rounded`):
#
#  1. The `is Array`/`is List` subclass delegation handed EVERY non-user method
#     to the instance's backing `__mutsu_array_storage`, a plain `Array`. That
#     is right for the Positional protocol and for rendering, but not for the
#     methods that answer *what the receiver is*: `.^name` answered `Array`,
#     `.isa(R)`/`.does(R)` answered False, `.^parents` named `List`.
#  2. `my @a is Alias` baked the trait name literally and probed the class
#     registry with it, so a `constant` alias for a class never tied.

plan 20;

class R is Array {}

# --- 1. type identity is the receiver's own, in every call form -------------
{
    is R.new(1, 2).^name, 'R', 'a chained .^name names the subclass';
    my $v = R.new(1, 2);
    is $v.^name, 'R', 'and so does the through-a-variable spelling';
    is R.new(1, 2).WHAT.^name, 'R', '.WHAT is the subclass type object';
    ok R.new(1, 2).isa(R), '.isa(the subclass) is True';
    ok R.new(1, 2).isa(Array), '.isa(the parent) is True too';
    ok R.new(1, 2).does(R), '.does(the subclass) is True';
    is R.new(1, 2).^parents.head.^name, 'Array', '.^parents names the real parent';
    is R.new(1, 2).WHICH.^name, 'ObjAt', '.WHICH is an ObjAt';
}

# --- 2. ... while the Positional protocol still delegates -------------------
{
    is R.new(1, 2).elems, 2, 'elems still delegates to the backing storage';
    is R.new(1, 2).gist, '[1 2]', 'gist renders as the array';
    is R.new(1, 2).Str, '1 2', 'Str renders as the array';
    is R.new(1, 2, 3)[1], 2, 'indexing still reaches the storage';
    ok R.new(1, 2).Bool, 'Bool is the array truthiness';
}

# --- 3. a `constant` alias for a class names the class ----------------------
{
    my constant C = R;
    is C.new(1, 2).^name, 'R', 'a same-file constant alias constructs the class';
    my @a is C = 1, 2, 3;
    is @a.^name, 'R', '`is <constant alias>` ties the variable to the class';
    is-deeply @a.List, (1, 2, 3), 'and the initializer still lands in it';
}

# --- 4. ... including one imported from another compunit --------------------
{
    is Rounded.new(1).^name, 'RoundedMod::Array::Rounded',
        'an imported constant alias constructs the aliased class';
    my @a is Rounded = 1, 2, 3;
    is @a.^name, 'RoundedMod::Array::Rounded',
        '`is <imported alias>` ties the variable to the aliased class';
    is-deeply @a.List, (1, 2, 3), 'and keeps the initializer';
}

# --- 5. a built-in variable trait is not shadowed by a same-named lexical ---
{
    my $default = R;
    my @a is default(7);
    is @a[3], 7, 'a lexical named `default` does not rename the built-in trait';
}
