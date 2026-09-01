use Test;

# ADR-0036 slice 4. A Pair BINDS its value (rakudo's `Pair` BUILD does
# `$!value := value`), so `$p.value = X`:
#   * assigns INTO the container when the value is a mutable Array/Hash,
#   * writes through the cell when the value is an element/scalar container,
#   * and otherwise dies, because a bare immutable value has nothing to assign
#     into.
# Every row below was checked against real raku.
#
# This is what replaced the `env`-wide search for "an array/hash whose element
# happens to compare equal to this pair's value", which is deleted.

plan 21;

# --- reference values assign INTO the container -----------------------------

{
    my @a = 1, 2;
    my $p = (a => @a);
    $p.value = (3, 4);
    is-deeply @a, [3, 4], 'assigning a list through .value list-assigns into the bound array';
    is $p.value.raku, '[3, 4]', 'the pair still holds that same Array';
}

{
    my %h = x => 1;
    my $p = (a => %h);
    $p.value = (y => 2);
    is-deeply %h, {y => 2}, 'assigning through .value replaces the bound hash contents';
}

{
    my @a;
    my $p = (a => @a);
    $p.value = 5;
    is-deeply @a, [5], 'a scalar assigned through .value list-assigns into an empty array';
}

{
    my $p = (a => [1, 2]);
    $p.value = [3, 4];
    is $p.gist, 'a => [3 4]', 'an anonymous Array value is assigned into, not rebound';
}

# --- element writes through the value keep ONE container --------------------

{
    my $h = { :a(1) };
    my $p = ($h => $h);
    $p.value<b> = 2;
    $p.value<c> = 3;
    is-deeply $h, {a => 1, b => 2, c => 3},
        'repeated element writes through .value all land in the source hash';
    ok $p.value =:= $h, 'the pair value and the source stay the same container';
}

# --- immutable values die ---------------------------------------------------

{
    my $p = (1 => "a");
    throws-like { $p.value = "z" }, X::Assignment::RO,
        'a Str-valued pair has nothing to assign into';
}

{
    my $p = (a => 1);
    throws-like { $p.value = 2 }, X::Assignment::RO,
        'an Int-valued pair has nothing to assign into';
}

{
    # A List is immutable: raku list-assigns into it and dies on the first
    # element.
    my $p = (a => (1, 2));
    throws-like { $p.value = 5 }, X::Assignment::RO,
        'a List-valued pair dies on its first element';
}

{
    # ... but an EMPTY list has no element to fail on, so the assignment is a
    # no-op rather than an error.
    my $p = (a => ());
    lives-ok { $p.value = 5 }, 'assigning into an empty List is a no-op';
    is $p.gist, 'a => ()', 'the empty List is unchanged';
}

# --- a captured scalar container still writes through -----------------------

{
    my $v = 1;
    my $p = (a => $v);
    $p.value = 2;
    is $v, 2, '`key => $var` aliases the variable container';
    is $p.value, 2, 'and the pair reads the new value';
}

# --- shaped arrays hand out element containers too --------------------------

{
    my @a[3];
    is @a.pairs[0].value.VAR.^name, 'Scalar',
        'a 1-D shaped array .pairs value is the element container';
    for @a.pairs -> $p { $p.value = 7 }
    is @a.raku, 'Array.new(:shape(3,), [7, 7, 7])',
        '1-D shaped .pairs writeback keeps the shape';
}

{
    my @a[1; 2];
    for @a.pairs -> $p { $p.value = $p.key[1] + 40 }
    is @a[0; 0], 40, 'multi-dim shaped .pairs writes through the leaf container';
    is @a[0; 1], 41, 'the tuple key selects the right leaf';
}

# --- a `:=`-bound element enforces the container element type ---------------
# The bind promotes the slot to a cell that carries the container's `of`-type,
# including when the slot did not exist yet and the bind is deferred.

{
    my Str @a;
    my $r := @a[2];
    throws-like { $r = 42 }, X::TypeCheck::Assignment,
        'a deferred array-element bind still checks the element type';
}

{
    my Int %h;
    my $r := %h<k>;
    throws-like { $r = "s" }, X::TypeCheck::Assignment,
        'a deferred hash-element bind still checks the element type';
}

{
    my Str @a;
    my $r := @a[2];
    $r = "x";
    is-deeply @a, Array[Str].new(Str, Str, "x"),
        'a well-typed deferred write fills the gap with the element type object';
}
