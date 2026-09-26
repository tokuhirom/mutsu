use Test;

# `BIND-KEY` / `ASSIGN-KEY` called as methods. Reduced from Array::Sparse,
# which keeps its elements in a `%!sparse` attribute and drives it only
# through these methods.

plan 11;

# ASSIGN-KEY writes into the hash node every alias shares.
{
    my %g;
    my %a := %g;
    %a.ASSIGN-KEY(1, 1);
    is-deeply %g, %(1 => 1), 'ASSIGN-KEY through a := alias reaches the source';
}

{
    my class C {
        has %!s;
        method via-alias { my %s := %!s; %s.ASSIGN-KEY(2, 2); %!s }
        method direct    { %!s.ASSIGN-KEY(3, 3); %!s.raku }
    }
    is-deeply C.new.via-alias, %(2 => 2), 'ASSIGN-KEY through an alias of an attribute';
    is C.new.direct, '{"3" => 3}', 'ASSIGN-KEY does not retype the attribute hash';
}

# A key bound to a bare value has no container to assign into.
{
    my %h;
    %h.BIND-KEY(3, 666);
    throws-like { %h.ASSIGN-KEY(3, 1) }, X::AdHoc,
        message => 'Cannot assign to an immutable value',
        'ASSIGN-KEY on a key BIND-KEY bound to a literal dies';
    throws-like { %h{3} = 2 }, X::AdHoc,
        'subscript assignment to a key BIND-KEY bound to a literal dies';
    is %h{3}, 666, 'the bound value survives both attempts';
}

{
    my %k;
    %k<b> := 5;
    throws-like { %k.ASSIGN-KEY('b', 2) }, X::AdHoc,
        'ASSIGN-KEY on a key bound by subscript := dies';
}

# Forwarding a sigilless parameter bound to a literal binds the bare value.
{
    my %h;
    sub bind-it($k, \value) { %h.BIND-KEY($k, value) }
    bind-it(2, 7);
    throws-like { %h.ASSIGN-KEY(2, 1) }, X::AdHoc,
        'BIND-KEY of a sigilless literal-bound parameter is read-only';
    is %h{2}, 7, 'and the bound value is intact';
}

# Binding a variable's container stays writable through either name.
{
    my %h;
    my $x = 1;
    %h.BIND-KEY('x', $x);
    %h.ASSIGN-KEY('x', 42);
    is $x, 42, 'ASSIGN-KEY writes through a key bound to a variable';
}

# Deleting the bound key removes its read-only-ness with it.
{
    my %h;
    %h.BIND-KEY('k', 1);
    %h<k>:delete;
    %h.ASSIGN-KEY('k', 2);
    is %h<k>, 2, 'a deleted then re-assigned key is writable';
}
