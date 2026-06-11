use Test;

# Phase 2 (element containers): binding a scalar to an array/hash element
# (`$x := @a[i]`) aliases the element by a shared cell, so writes through
# either side are visible to the other — including through arbitrarily deep
# `$struct[..]<..>[..]` paths (which the old index-back-reference lost when an
# enclosing container was COW-cloned on a later write).

plan 54;

# Single-level array element
{
    my @a = 1, 2, 3;
    my $x := @a[0];
    is $x, 1, 'bound element reads the current value';
    @a[0] = 99;
    is $x, 99, 'write to the element is visible through the binding';
    $x = 5;
    is @a[0], 5, 'write through the binding is visible in the element';
}

# Two-level array
{
    my $s = [0, [10, 20]];
    my $x := $s[1][1];
    is $x, 20, 'deep (2-level) bound element reads';
    $s[1][1] = 99;
    is $x, 99, '2-level: element write -> binding';
    $x = 5;
    is $s[1][1], 5, '2-level: binding write -> element';
}

# Deep mixed array/hash path (the nested.t shape)
{
    my $struct = [
        "ignored",
        { key => { subkey => [ "ignored", 42 ] } },
    ];
    my $abbrev := $struct[1]<key><subkey>[1];
    is $abbrev, 42, 'deep array/hash path bound element reads';
    $struct[1]<key><subkey>[1] = 43;
    is $abbrev, 43, 'deep: element write -> binding (survives COW of the path)';
    $abbrev = 44;
    is $struct[1]<key><subkey>[1], 44, 'deep: binding write -> element';
}

# The bound element stays transparent in value contexts (no cell leak)
{
    my @a = 1, 2, 3;
    my $x := @a[1];
    is @a.sum, 6, 'sum sees the element value, not the cell';
    is @a.reduce(&[+]), 6, 'reduce sees the element value';
    is @a.raku, '[1, 2, 3]', '.raku does not leak the cell';
    is (@a[1] + 10), 12, 'arith on the element reads through';
}

# Single-level hash element binding (Stage 1)
{
    my %h = a => 1, b => 2;
    my $x := %h<a>;
    is $x, 1, 'bound hash element reads the current value';
    %h<a> = 99;
    is $x, 99, 'hash element write -> binding';
    $x = 5;
    is %h<a>, 5, 'binding write -> hash element';
}

# Deep hash-in-hash element binding survives COW of the inner hash
{
    my %g = outer => { inner => 10 };
    my $y := %g<outer><inner>;
    is $y, 10, 'deep hash leaf bound element reads';
    %g<outer><inner> = 55;
    is $y, 55, 'deep hash: element write -> binding (survives COW)';
    $y = 7;
    is %g<outer><inner>, 7, 'deep hash: binding write -> element';
}

# A hash value that is a callable, invoked mid-path, is NOT promoted to a cell
{
    my $inner = { subkey => [ "ignored", 42 ] };
    my sub getit() is raw { $inner }
    my $struct = [ "ignored", { key => &getit } ];
    my $abbrev := $struct[1]<key>()<subkey>[1];
    is $abbrev, 42, 'callable invoked mid bind-path stays callable (no over-promotion)';
}

# Bound hash element stays transparent in value contexts (no cell leak)
{
    my %h = a => 1, b => 2, c => 3;
    my $x := %h<a>;
    %h<a> = 10;
    is %h.values.sort.join(','), '2,3,10', 'hash iteration does not leak the cell';
    is %h<a> + 5, 15, 'arith on the bound hash element reads through';
}

# Deferred binding to a missing nested key: does not autovivify, and `=:=`
# identity holds after the assignment promotes the element to a cell.
{
    my %h;
    my $b := %h<a><b>;
    is %h.keys.elems, 0, 'deferred bind does not autovivify';
    $b = 42;
    is %h<a><b>, 42, 'deferred bind autovivifies on assignment';
    ok %h<a><b> =:= $b, 'deferred-bound element keeps identity (=:=) after promotion';
}

# LHS binding: a deep element bound to a scalar var (`element := $v`) — writes
# through either side reach the other (symmetric form of RHS element binding).
{
    my $struct = [
        "ignored",
        { key => { subkey => [ "ignored", 42 ] } },
    ];
    my $abbrev = 30;
    $struct[1]<key><subkey>[1] := $abbrev;
    is $abbrev, 30, 'LHS bind leaves the source value unchanged';
    $struct[1]<key><subkey>[1] = 31;
    is $abbrev, 31, 'LHS deep: element write -> source (survives COW)';
    $abbrev = 32;
    is $struct[1]<key><subkey>[1], 32, 'LHS deep: source write -> element';
}

# LHS binding through a deep array-of-array path
{
    my $s = [0, [10, [100, 200]]];
    my $v = 7;
    $s[1][1][0] := $v;
    $s[1][1][0] = 8;
    is $v, 8, 'LHS deep array: element write -> source';
    $v = 9;
    is $s[1][1][0], 9, 'LHS deep array: source write -> element';
    is $s[1][1][1], 200, 'sibling element untouched';
}

# Copying an array with a bound element SNAPSHOTS the value (Raku `=`
# decontainerizes), so a later write through the source does not leak into
# the copy — for both LHS (`@a[i] := $v`) and RHS (`$x := @a[i]`) binds.
{
    my @array = <a b c>;
    my $var = "d";
    @array[1] := $var;       # LHS bind (single level)
    $var = "e";
    my @copy = @array;
    $var = "f";
    is ~@copy, "a e c", 'LHS-bound element: array copy snapshots the value';
    is ~@array, "a f c", 'LHS-bound element: original still aliases the source';

    my @a2 = 1, 2, 3;
    my $x := @a2[1];         # RHS bind
    $x = 20;
    my @copy2 = @a2;
    $x = 30;
    is ~@copy2, "1 20 3", 'RHS-bound element: array copy snapshots the value';
    is ~@a2, "1 30 3", 'RHS-bound element: original still aliases the source';
}

# Binding to a *container-valued* element (the leaf holds a Hash/Array) must
# promote the leaf to a shared cell too, so deep writes through either side are
# mutually visible.
{
    my %h = key => { inner => 5 };
    my $x := %h<key>;
    is $x<inner>, 5, 'container-leaf bind: initial read';
    %h<key><inner> = 50;
    is $x<inner>, 50, 'container-leaf bind: source write -> element';
    $x<inner> = 500;
    is %h<key><inner>, 500, 'container-leaf bind: element write -> source';
}

# RHS deep container-leaf bind: the bound root is itself a cell, so writes in
# both directions must traverse it.
{
    my $foo = [ "ig", { key => { subkey => [ "ig", 2 ] } } ];
    my $x := $foo[1]<key>;
    is $x<subkey>[1], 2, 'deep container-leaf: initial read';
    $foo[1]<key><subkey>[1] = 7;
    is $x<subkey>[1], 7, 'deep container-leaf: source write -> element';
    $x<subkey>[1] = 8;
    is $foo[1]<key><subkey>[1], 8, 'deep container-leaf: element write -> source';
}

# Element-to-element bind: bind a deep element of one structure to a deep
# element of another (roast S03-binding/nested.t 32-33 shape). Writes in both
# directions propagate through the shared cell.
{
    my $foo = [ "ig", { key => { subkey => [ "ig", 2 ] } } ];
    my $bar = [ "ig", { key => { subkey => [ "ig", 5 ] } } ];
    $bar[1]<key><subkey> := $foo[1]<key>;
    is $bar[1]<key><subkey><subkey>[1], 2, 'element-element: initial read';
    $foo[1]<key><subkey>[1] = 7;
    is $bar[1]<key><subkey><subkey>[1], 7, 'element-element: source write -> element';
    $bar[1]<key><subkey><subkey>[1] = 8;
    is $foo[1]<key><subkey>[1], 8, 'element-element: element write -> source';
}

# Self-referential (cyclic) bind: bind an element of a structure to an element
# of the SAME structure (nested.t 35-37). Writes must terminate and propagate
# both ways through the cycle.
{
    my $struct = [ "ig", { key => { foo => "bar", subkey => [ "ig", 100 ] } } ];
    $struct[1]<key><subkey>[1] := $struct[1]<key>;
    is $struct[1]<key><subkey>[1]<foo>, "bar", 'cyclic bind: initial read';
    $struct[1]<key><subkey>[1]<foo> = "new_value";
    is $struct[1]<key><foo>, "new_value", 'cyclic bind: long write -> short read';
    $struct[1]<key><foo> = "very_new_value";
    is $struct[1]<key><subkey>[1]<foo>, "very_new_value", 'cyclic bind: short write -> long read';
}

# A bound container-valued element must NOT leak the `ContainerRef` cell into
# rendering / iteration of the enclosing structure (Phase 5).
{
    my @a = [{x => 1}, {y => 2}, {z => 3}];
    my $b := @a[1];
    is @a.raku, '[{:x(1)}, {:y(2)}, {:z(3)}]', 'no cell leak in array .raku';
    is @a.list[1].WHAT.^name, 'Hash', 'no cell leak in array .list element type';
    my %h = a => {n => 1}, b => {n => 2};
    my $c := %h<a>;
    is %h<a>.WHAT.^name, 'Hash', 'no cell leak in hash element read type';
}

# Binding to an element reached *through an `is raw` sub call* in the middle of
# the path (`$struct[1]<key>()<subkey>[1]`, nested.t 11-12). The sub returns the
# inner structure raw, so the element shares a cell with the source; a write
# reached via the stack-target generic handler must go THROUGH that cell.
{
    my $inner = { subkey => [ "ig", 42 ] };
    my sub get_inner () is raw { $inner }
    my $struct = [ "ig", { key => &get_inner } ];

    is $struct[1]<key>()<subkey>[1], 42, 'sub-mid-path: sanity read';
    my $abbrev := $struct[1]<key>()<subkey>[1];
    is $abbrev, 42, 'sub-mid-path: bind read';
    $struct[1]<key>()<subkey>[1] = 43;
    is $abbrev, 43, 'sub-mid-path: source write -> bind';
    $abbrev = 44;
    is $struct[1]<key>()<subkey>[1], 44, 'sub-mid-path: bind write -> source';
}

# vim: expandtab shiftwidth=4
