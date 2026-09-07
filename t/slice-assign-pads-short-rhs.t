use Test;

plan 20;

# A slice assignment is a LIST assignment: raku zips the RHS against the
# targeted slots, and a slot past the end of the RHS gets the container's
# UNDEFINED value. mutsu broadcast the RHS on the associative side and padded
# with `Nil` on the positional side.

{
    my %h; %h{(1, 2)} = "z";
    is %h.raku, '{"1" => "z", "2" => Any}', 'a one-element RHS does not fill the whole hash slice';
}
{
    my %i; %i<a b> = 1;
    is %i.raku, '{:a(1), :b(Any)}', 'the angle-bracket spelling too';
}
{
    my %j; %j{(1, 2, 3)} = "z", "y";
    is %j.raku, '{"1" => "z", "2" => "y", "3" => Any}', 'the LAST RHS value does not repeat either';
}
{
    my @a; @a[0, 1, 2] = "z",;
    is @a.raku, '["z", Any, Any]', 'a positional slice pads with Any, not Nil';
}
{
    my @s[3]; @s[0, 1, 2] = "z",;
    is @s.raku, 'Array.new(:shape(3,), ["z", Any, Any])', 'a shaped slice pads the same way';
}

# A typed target pads with its own element type.
{
    my Int %t; %t<a b> = 1;
    is %t<b>.^name, 'Int', 'a typed hash pads with the value type object';
    nok %t<b>.defined, 'and it is undefined';
}
{
    my Int @u; @u[0, 1] = 1;
    is @u.raku, 'Array[Int].new(1, Int)', 'a typed array pads with the element type object';
}
{
    my int @n; @n[0, 1] = 1;
    is @n.raku, 'array[int].new(1, 0)', 'a native int array pads with 0';
}
{
    my str @n; @n[0, 1] = "x";
    is @n.raku, 'array[str].new("x", "")', 'a native str array pads with the empty string';
}
{
    my num @n; @n[0, 1] = 1e0;
    is @n.raku, 'array[num].new(1e0, 0e0)', 'a native num array pads with 0e0';
}

# An RHS longer than the slice drops the extra values, with no error.
{
    my @b; @b[0, 1] = 1, 2, 3;
    is @b.raku, '[1, 2]', 'a longer RHS is truncated';
}
{
    my @l; @l[0, 1] = <a b c d>;
    is @l.raku, '["a", "b"]', 'the same for a word list';
}

# An empty RHS pads every slot.
{
    my @c; @c[0, 1] = ();
    is @c.raku, '[Any, Any]', 'an empty RHS leaves every positional slot undefined';
}
{
    my %f; %f<a b> = ();
    is %f.raku, '{:a(Any), :b(Any)}', 'and every associative one';
}

# A Seq RHS is eager for a slice assignment (ADR-0058) and pads the same way.
{
    my %s; %s<a b c> = (1, 2).Seq;
    is %s.raku, '{:a(1), :b(2), :c(Any)}', 'a Seq RHS pads its short tail';
}

# The HYPER spelling is the opposite rule and keeps it: it CYCLES the RHS
# across the targets, which is the whole point of the metaoperator. Spelled
# with the Unicode delimiters because rakudo does not parse the ASCII
# `>>=>>` form in this position.
{
    my %h; %h<a b c> »=» 7;
    is %h.raku, '{:a(7), :b(7), :c(7)}', 'a hyper slice assignment broadcasts a scalar';
}
{
    my %i; %i<a b c> »=» (1, 2);
    is %i.raku, '{:a(1), :b(2), :c(1)}', 'and cycles a short list';
}
{
    my @a; @a[0, 1, 2] »=» 5;
    is @a.raku, '[5, 5, 5]', 'the positional hyper spelling too';
}
{
    my %j; my @k = <a b c>; %j{@k} »=» 9;
    is %j.raku, '{:a(9), :b(9), :c(9)}', 'and with a runtime key list';
}
