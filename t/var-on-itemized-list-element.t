use Test;

# `.VAR` on an itemized list element used to report the INNER type:
#
#     (1, 2, 3, $(4, 5))[3].VAR.^name   # raku: Scalar   mutsu: List
#
# The `$(...)` itemization is exactly what puts a Scalar container around the
# inner list, and `.VAR` is the one introspection that is supposed to report it
# -- that is the point of the doc example this came from
# (`raku-doc/doc/Language/structures.rakudoc`, "itemization is what makes a
# list one element"). Every expectation below was measured against raku
# v2026.07 first.
#
# There is a standing warning that `.VAR` alone is not a sound test that a
# container arrived (it reports Scalar whether or not one did), so the
# itemization is checked through `.raku`, `.elems` and `.WHAT` as well.

plan 29;

# --- the doc example ------------------------------------------------------

is (1, 2, 3, $(4, 5))[3].VAR.^name, 'Scalar', 'an itemized list element reports Scalar';
is (1, 2, 3, $(4, 5))[3].VAR.WHAT.^name, 'Scalar', '... and .VAR.WHAT agrees';
is (1, 2, 3, $(4, 5))[3].raku, '$(4, 5)', 'the itemization still shows in .raku';
is (1, 2, 3, $(4, 5))[3].elems, 2, '... and the element is still the whole list';
is (1, 2, 3, $(4, 5))[3].WHAT.^name, 'List', '.WHAT decontainerizes, as in raku';

# The same value read through a variable was always right; it must stay right.
{
    my $e = (1, 2, 3, $(4, 5))[3];
    is $e.VAR.^name, 'Scalar', 'via a variable, too';
    is $e.raku, '$(4, 5)', '... with the itemization intact';
}

# --- all three itemization spellings --------------------------------------

is (1, $[2, 3])[1].VAR.^name, 'Scalar', 'an itemized Array element';
is (1, ${a => 1})[1].VAR.^name, 'Scalar', 'an itemized Hash element';
is (1, ${a => 1})[1].raku, '${:a(1)}', '... with the itemization intact';

# --- a NON-itemized element must keep its own type ------------------------

is (1, 2)[0].VAR.^name, 'Int', 'a plain List element has no container';
is ((1, 2), (3, 4))[0].VAR.^name, 'List', 'a plain sub-list element is a List';
is ((1, 2), (3, 4))[0].raku, '(1, 2)', '... and is not itemized';

# --- a slice hands back a List of elements, and .VAR on a List is identity --

is (1, 2, 3)[0, 1].VAR.^name, 'List', 'a slice of a List reports List';
{
    my @c = 1, 2, 3;
    is @c[0, 1].VAR.^name, 'List', 'a slice of an Array reports List';
}

# --- real Array / Hash elements are containers regardless -----------------

{
    my @a = 1, 2;
    is @a[0].VAR.^name, 'Scalar', 'an Array element is a container';
    my @b = (1, 2), (3, 4);
    is @b[0].VAR.^name, 'Scalar', 'an Array element holding a list, too';
    is @b[0].raku, '$(1, 2)', '... and an Array itemizes what it stores';
    my %h = a => 1;
    is %h<a>.VAR.^name, 'Scalar', 'a Hash element is a container';
}

# Storing an already-itemized value into a real container.
{
    my %g;
    %g<a> = $(1, 2);
    is %g<a>.VAR.^name, 'Scalar', 'an itemized value stored into a Hash';
    is %g<a>.raku, '$(1, 2)', '... keeps its itemization';
}

# --- chained and literal subscripts ---------------------------------------

{
    my %d;
    %d<a><b> = 5;
    is %d<a><b>.VAR.^name, 'Scalar', 'a chained Hash subscript';
    my @g = [1, 2], [3, 4];
    is @g[0][1].VAR.^name, 'Scalar', 'a chained Array subscript';
}
is [1, 2][0].VAR.^name, 'Scalar', 'a literal Array subscript';
is (1, 2)[0].VAR.^name, 'Int', 'a literal List subscript stays the element type';

# --- .VAR metadata on a real named container is untouched -----------------

{
    my @n = 1, 2;
    is @n.VAR.name, '@n', '.VAR.name on a named Array';
    is @n.VAR.^name, 'Array', '.VAR.^name on a named Array is its own type';
    my $s = 5;
    is $s.VAR.^name, 'Scalar', '.VAR.^name on a named Scalar';
    is 5.VAR.^name, 'Int', '.VAR.^name on a bare literal is the value type';
}
