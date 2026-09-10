use Test;

# A `:=` bind of an element to an immutable literal makes that element
# read-only: a later plain `=` assignment must throw and preserve the value.
# See PLAN.md §8.7 / T-051 (Hash::Agnostic tied hash).

plan 20;

# --- plain hash element ---------------------------------------------------
{
    my %h;
    %h<i> := 137;
    is %h<i>, 137, 'hash element bound to literal reads back the literal';
    dies-ok { %h<i> = 666 }, 'assign to literal-bound hash element dies';
    is %h<i>, 137, 'hash element value preserved after failed assign';
    is %h<i>:delete, 137, 'literal-bound hash element deletes to its value';
}

# --- plain array element --------------------------------------------------
{
    my @a;
    @a[0] := 137;
    is @a[0], 137, 'array element bound to literal reads back the literal';
    dies-ok { @a[0] = 9 }, 'assign to literal-bound array element dies';
    is @a[0], 137, 'array element value preserved after failed assign';
}

# --- string literal bind --------------------------------------------------
{
    my %s;
    %s<k> := "hi";
    dies-ok { %s<k> = "bye" }, 'assign to string-literal-bound element dies';
    is %s<k>, "hi", 'string-literal-bound element value preserved';
}

# --- other keys stay writable --------------------------------------------
{
    my %h;
    %h<i> := 137;
    %h<j> = 5;
    is %h<j>, 5, 'a different (unbound) key stays writable';
    lives-ok { %h<j> = 6 }, 'reassigning an unbound key lives';
    is %h<j>, 6, 'unbound key reassignment takes effect';
}

# --- whole-container reassignment breaks the binding ---------------------
{
    my %h;
    %h<i> := 137;
    %h = (i => 5, j => 9);
    lives-ok { %h<i> = 666 }, 'whole-hash reassign makes the element writable again';
    is %h<i>, 666, 'element writable after whole-hash reassign';
}
{
    my @a;
    @a[0] := 137;
    @a = (1, 2, 3);
    lives-ok { @a[0] = 9 }, 'whole-array reassign makes the element writable again';
    is @a[0], 9, 'element writable after whole-array reassign';
}

# --- delete frees the read-only marker -----------------------------------
{
    my %g;
    %g<x> := 10;
    %g<x>:delete;
    lives-ok { %g<x> = 99 }, 'a deleted literal-bound key becomes writable';
    is %g<x>, 99, 'reassignment after delete takes effect';
}

# --- binding to a mutable variable stays writable-through ----------------
{
    my $v = 5;
    my %h;
    %h<k> := $v;
    lives-ok { %h<k> = 6 }, 'element bound to a mutable variable stays writable';
    is %h<k>, 6, 'write-through updates the element';
}
