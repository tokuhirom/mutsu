use Test;

# ADR-0036 slice 4: an element promoted to its own container cell carries the
# CONTAINER's element type constraint, so a write through any alias is checked
# exactly as a direct `@a[0] = v` store is -- and the failure is REPORTED the
# way raku reports it, naming the container rather than coming out as a bare
# "Type check failed in assignment" with no symbol at all.

plan 26;

# --- the bind alias -------------------------------------------------------
{
    my Str @a = "x", "y";
    dies-ok { my $r := @a[0]; $r = 42 }, 'a `:=`-bound element rejects a bad write';
    is-deeply @a, Array[Str].new("x", "y"), '... and the array is unchanged';

    my Str @b = "x", "y";
    lives-ok { my $r := @b[0]; $r = "z" }, 'a well-typed write through the bind lands';
    is-deeply @b, Array[Str].new("z", "y"), '... and reaches the element';
}

{
    my Int %h = a => 1;
    dies-ok { my $r := %h<a>; $r = "s" }, 'a `:=`-bound hash element rejects a bad write';
    is %h<a>, 1, '... and the hash is unchanged';

    my Int %g = a => 1;
    lives-ok { my $r := %g<a>; $r = 9 }, 'a well-typed write through the hash bind lands';
    is %g<a>, 9, '... and reaches the element';
}

# --- the subscript-adverb Pair -------------------------------------------
{
    my Int @a = 1, 2;
    dies-ok { my $p = @a[0]:p; $p.value = "s" }, '`:p`\'s pair value is constrained';
    is-deeply @a, Array[Int].new(1, 2), '... and the array is unchanged';

    my Int @b = 1, 2;
    lives-ok { my $p = @b[0]:p; $p.value = 9 }, 'a well-typed write through `:p` lands';
    is-deeply @b, Array[Int].new(9, 2), '... and reaches the element';
}

# --- the `for`-loop element alias (ADR-0045 row 28) ----------------------
# This path knows the source variable, so the message names it the way raku
# does rather than falling back to the bare sigil.
{
    my $err;
    my Int @a = 1, 2;
    try { for @a -> $v is rw { $v = "s" }; CATCH { default { $err = $_ } } };
    isa-ok $err, X::TypeCheck::Assignment, 'the `for` alias raises X::TypeCheck::Assignment';
    is $err.message,
        'Type check failed for an element of @a; expected Int but got Str ("s")',
        '... naming the container, not the alias';
    is $err.expected, Int, '.expected is the element type';
    is $err.got, "s", '.got is the offending value';
    is-deeply @a, Array[Int].new(1, 2), '... and the array is unchanged';

    my Int %h = a => 1;
    my $herr;
    try { for %h.values -> $v is rw { $v = "s" }; CATCH { default { $herr = $_ } } };
    is $herr.message,
        'Type check failed for an element of %h; expected Int but got Str ("s")',
        'a typed hash names itself too';
}

# --- an UNTYPED container must keep accepting anything -------------------
{
    my @a = 1, 2;
    lives-ok { my $r := @a[0]; $r = "s" }, 'an untyped array element takes any value';
    is-deeply @a, ["s", 2], '... and the write lands';

    my %h = a => 1;
    lives-ok { my $r := %h<a>; $r = "s" }, 'an untyped hash element takes any value';
    is %h<a>, "s", '... and the write lands';

    lives-ok { my @b = 1, 2; for @b -> $v is rw { $v = "s" } },
        'an untyped `for` alias takes any value';
}

# --- a producer-handed cell is named by the loop too ----------------------
# `vm_element_producers.rs` sees a receiver value, not a variable, so the cell
# it hands out starts with the bare sigil; the loop retags it.
{
    my $err;
    my Int @a = 1, 2;
    try { for @a.values -> $v is rw { $v = "s" }; CATCH { default { $err = $_ } } };
    is $err.message,
        'Type check failed for an element of @a; expected Int but got Str ("s")',
        '`.values` blames the source array';

    my $rerr;
    my Int @b = 1, 2;
    try { for @b.reverse -> $v is rw { $v = "s" }; CATCH { default { $rerr = $_ } } };
    is $rerr.message,
        'Type check failed for an element of @b; expected Int but got Str ("s")',
        '`.reverse` blames the source array';
}

# --- a plain typed SCALAR keeps its own wording --------------------------
# Its cell is not an element of anything, so raku reports the assignment form.
{
    my $err;
    try { my Int $x; my $r := $x; $r = "s"; CATCH { default { $err = $_ } } };
    is $err.message,
        'Type check failed in assignment to $x; expected Int but got Str ("s")',
        'a typed scalar cell keeps the assignment wording';
}
