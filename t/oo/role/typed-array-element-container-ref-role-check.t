use Test;

# WebDriver2 0.1.12 (ecosystem sweep): a role-typed array declared from a
# source array/hash's `.values` (`my ANode @x = %children.values`) rejected
# an element that genuinely composes the constrained role, with "expected
# ANode but got Any".
#
# Root cause: an array slot (and a hash entry) can hold a `ContainerRef` cell
# rather than a bare value (e.g. any element read back after being stored via
# `%h{$k} = $v`). The per-element check for a typed `@`-array
# (`array_elements_match_constraint`, src/vm/vm_misc_typed_range.rs) recurses
# down to `type_matches_value` for a non-container-shaped element, but that
# function derefed a `Scalar` wrapper without also derefing a `ContainerRef`/
# `ContainerView` — so the opaque cell fell through every Instance-shaped
# check and reported as "Any". It reproduced only through a role satisfied
# INDIRECTLY (a role composing another role, `AFrame does ANode`), because a
# direct match short-circuits earlier via the container's own tag-match fast
# path in some call shapes; going through `.values()` always hits the slow
# per-element path regardless.

plan 4;

role ANode { has Str $.name is required; }
role AFrame does ANode { }
class Elem does ANode { }
class Fr does AFrame { }

{
    my ANode %children;
    %children{'iframe'} = Fr.new(name => 'iframe');
    %children{'text'} = Elem.new(name => 'text');
    my ANode @vals = %children.values;
    is @vals.elems, 2, 'typed array built from a role-typed Hash.values keeps both elements';
}

{
    # The Array side of the same bug: a plain array's element, read back via
    # `.values`, must also decontainerize before the element-type check.
    my @a = (Fr.new(name => 'iframe'),);
    my ANode @vals = @a.values;
    is @vals.elems, 1, 'typed array built from Array.values keeps an indirect-role element';
}

{
    # Direct hash-element access (no `.values`) was never broken; pin it
    # alongside the regression so a future fix cannot regress this path.
    my ANode %children;
    %children{'iframe'} = Fr.new(name => 'iframe');
    is %children{'iframe'}.WHAT, Fr, 'direct hash element access still returns the stored instance';
}

{
    # A container's aliased element (`:=`) is the other common source of a
    # `ContainerRef`-shaped array element; the same typed-array check must
    # accept it too.
    my $shared = Fr.new(name => 'shared');
    my @a := (my $x := $shared, );
    my ANode @vals = @a.values;
    is @vals.elems, 1, 'typed array built from an aliased element also keeps an indirect-role element';
}
