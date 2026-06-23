# First-class container identity (Phase 2) — round-out coverage.
#
# Raku Arrays/Hashes are reference types: storing one in a *scalar* slot (a `$`
# variable, an array element, a hash value, a scalar parameter) shares the same
# underlying container, so structural mutations (.push/.unshift/element assign)
# made through any holder are visible to all. Element/whole-container binds
# (`:=`) and `=`-sharing both resolve to a shared `ContainerRef` cell that
# survives COW clones. This file locks the cases that round out the existing
# scalar-array-share / element-array-share / nested-bind suites.
use Test;

plan 18;

# --- Hash-of-Hash deep share through a scalar bind/assignment ---
{
    my %inner = (a => 1);
    my %h;
    %h<x> = %inner;
    %inner<b> = 2;
    is %h<x><b>, 2, 'HoH: mutation of the source hash is visible through the stored value';

    my %g = (x => {a => 1});
    my $r = %g<x>;
    $r<b> = 2;
    is %g<x><b>, 2, 'HoH: mutation through a scalar holder reaches the hash value';
}

# --- Array stored as a hash value, mutated through a scalar holder ---
{
    my %h = (k => [1, 2]);
    my $r = %h<k>;
    $r.push(3);
    is %h<k>.elems, 3, 'array-in-hash: push through scalar holder reaches the hash';
    is %h<k>[2], 3, 'array-in-hash: pushed value present';
}

# --- AoA row shared via a scalar holder ---
{
    my @a = ([1, 2], [3, 4]);
    my $r = @a[0];
    is $r.elems, 2, 'AoA: scalar holder reads the row';
    $r.push(9);
    is @a[0].elems, 3, 'AoA: push through holder reaches the array element';
}

# --- read-through: a shared scalar holder works as a list in method chains ---
{
    my @a = (1, 2, 3);
    my $s = @a;
    is $s.map(* + 1).join(","), "2,3,4", 'shared holder: .map reads through the cell';
    is $s.sum, 6, 'shared holder: .sum reads through the cell';
    is $s.elems, 3, 'shared holder: .elems reads through the cell';
}

# --- for-rw over an AoA mutates each row in place ---
{
    my @a = ([1, 2], [3, 4]);
    for @a -> $row { $row.push(0) }
    is-deeply @a, [[1, 2, 0], [3, 4, 0]], 'for-rw: each row mutated in place';
}

# --- typed array shares through a scalar slot ---
{
    my Int @a = (1, 2);
    my $n = @a;
    $n.push(3);
    is @a.elems, 3, 'typed array: push through scalar holder reaches the array';
}

# --- a snapshot taken with @ stays independent (copy semantics preserved) ---
{
    my @z = (1, 2);
    my @copy = @z;
    @z.push(9);
    is @copy.elems, 2, '@-assignment is a copy, not a share';
}

# --- chained scalar shares all see the same container ---
{
    my @z = (1, 2);
    my $a = @z;
    my $b = $a;
    $b.push(9);
    is @z.elems, 3, 'chained scalar share: mutation through the tail holder reaches the array';
}

# --- deep deferred bind (HashEntryRef path) materializes + aliases ---
{
    my %h;
    my $b := %h<a><b><c>;
    $b = 5;
    is %h<a><b><c>, 5, 'deep deferred bind: write materializes the path';
    %h<a><b><c> = 7;
    is $b, 5 + 2, 'deep deferred bind: bound var sees a later write through the hash';
}

# --- element-element bind: a later write to the source reaches the target ---
{
    my @a = (1, 2, 3);
    my @b = (4, 5, 6);
    @a[0] := @b[1];
    @b[1] = 99;
    is @a[0], 99, 'element-element bind: source write reaches the bound element';
}

# --- nested struct: scalar holder of a hash value mutates the nested hash ---
{
    my %h = (k => {n => 1});
    my $r = %h<k>;
    $r<m> = 2;
    is %h<k><n> + %h<k><m>, 3, 'nested struct: holder mutation reaches the nested hash';
}

# --- whole-container `:=` into a hash value, source write-through ---
{
    my %h;
    my @inner = (1, 2);
    %h<k> := @inner;
    @inner.push(9);
    is %h<k>.elems, 3, 'whole-container bind into hash value: source push reaches it';
}
