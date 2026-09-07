use Test;

plan 30;

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

# The HYPER spelling is the opposite rule and keeps it: it distributes its RHS
# across the target's shape, which is the whole point of the metaoperator.
# Spelled with the Unicode delimiters because rakudo does not parse the ASCII
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

# The rest of the hyper rule, which a slice store cannot express at all: `»=»`
# distributes across the target's SHAPE, so it applies to a whole container and
# to a computed index list just as much as to a literal slice. It used to lower
# to a plain `target = value`, which stored a ONE-element array for the whole-
# container spelling and left the literal slice to be filled by the store
# broadcasting a short RHS.
{
    my @a = 1, 2, 3; @a »=» 7;
    is @a.raku, '[7, 7, 7]', 'a hyper assignment over a whole array keeps its length';
}
{
    my %h = a => 1, b => 2; %h »=» 7;
    is %h.raku, '{:a(7), :b(7)}', 'and over a whole hash keeps its keys';
}
{
    my @a = 1, 2, 3; @a[0..2] »=» 7;
    is @a.raku, '[7, 7, 7]', 'a Range subscript distributes too';
}
{
    my @a = 1, 2, 3; @a[*] »=» 7;
    is @a.raku, '[7, 7, 7]', 'and a Whatever subscript';
}

# A hyper assignment has to know the slice's ARITY, which an undefined scalar's
# associative slice read used to collapse to a single `Any`. Its positional twin
# (`$h[0,1,2]`) was already right.
{
    my $h;
    is $h<a b c>.raku, '(Any, Any, Any)',
        'an associative slice of an undefined scalar has the slice arity';
}
{
    my $h; $h<a b c> »=» 42;
    is ($h<a>, $h<b>, $h<c>).raku, '(42, 42, 42)',
        'so a hyper assignment through one fills every key';
}

# A slice assignment's own VALUE is the list it actually stored -- the RHS
# zipped against the slots, so both padded and truncated -- not the raw RHS.
{
    my %h;
    is (%h<a b c> = 1, 2).raku, '(1, 2, Any)', 'a hash slice rvalue is padded';
}
{
    my %h;
    is (%h<a b> = 1, 2, 3).raku, '(1, 2)', 'a hash slice rvalue is truncated';
}
{
    my @a;
    is (@a[0, 1, 2] = 5,).raku, '(5, Any, Any)', 'an array slice rvalue is padded';
}
{
    my Int @a;
    is (@a[0, 1, 2] = 5,).raku, '(5, Int, Int)',
        'and a typed one pads its rvalue with the element type';
}
