use Test;

plan 28;

# A subscript store into a `$` that already holds a DEFINED value follows
# rakudo's Positional/Associative protocol: a value that does not do the
# subscript's role refuses the store. mutsu instead replaced the value with a
# fresh container (`my $s = 42; $s<k> = 5` left `$s` as `{k => 5}`) or, for an
# Array, answered "Index out of bounds".

# --- associative subscript: X::AdHoc, rakudo's Any.AT-KEY wording ------------

{
    my $s = (1, 2, 3).Seq;
    throws-like { $s<k> = 5 }, X::AdHoc,
        message => 'Type Seq does not support associative indexing.',
        'a Seq refuses an associative store';
    throws-like { $s{'k'} = 5 }, X::AdHoc,
        message => 'Type Seq does not support associative indexing.',
        'and the brace spelling of it';
}

{
    my $s = (1, 2, 3);
    throws-like { $s<k> = 5 }, X::AdHoc,
        message => 'Type List does not support associative indexing.',
        'a List refuses one';
}

{
    my $s = [1, 2, 3];
    throws-like { $s<k> = 5 }, X::AdHoc,
        message => 'Type Array does not support associative indexing.',
        'an Array refuses one -- it does Positional, not Associative';
    is $s.elems, 3, 'and is left alone';
}

{
    my $s = 1 .. 3;
    throws-like { $s<k> = 5 }, X::AdHoc,
        message => 'Type Range does not support associative indexing.',
        'a Range answers the protocol error, not an immutability one';
}

{
    my $s = 42;
    throws-like { $s<k> = 5 }, X::AdHoc,
        message => 'Type Int does not support associative indexing.',
        'an Int refuses one';
    is $s, 42, 'and is not replaced by a Hash';
}

{
    my $s = "str";
    throws-like { $s<k> = 5 }, X::AdHoc,
        message => 'Type Str does not support associative indexing.',
        'a Str refuses one';
    is $s, "str", 'and is not replaced by a Hash';
}

# The same through a `:=` alias to the scalar.
{
    my $s = 42;
    my $r := $s;
    throws-like { $r<k> = 5 }, X::AdHoc,
        message => 'Type Int does not support associative indexing.',
        'an alias to the scalar refuses it too';
    is $s, 42, 'and the aliased scalar is untouched';
}

# --- positional subscript: X::Assignment::RO naming the value ---------------

{
    my $s = 42;
    throws-like { $s[0] = 5 }, X::Assignment::RO,
        message => 'Cannot modify an immutable Int (42)',
        'a positional store into an Int is refused';
    is $s, 42, 'and is not replaced by an Array';
}

{
    my $s = "str";
    throws-like { $s[0] = 5 }, X::Assignment::RO,
        message => 'Cannot modify an immutable Str (str)',
        'a positional store into a Str is refused';
    is $s, "str", 'and is not replaced by an Array';
}

{
    my $s = 1 .. 3;
    throws-like { $s[0] = 5 }, X::Assignment::RO,
        message => 'Cannot modify an immutable Range (1..3)',
        'a Range DOES do Positional, so it is refused as immutable';
}

{
    my $s = (1, 2, 3);
    throws-like { $s[0] = 5 }, X::Assignment::RO,
        message => 'Cannot modify an immutable List ((1 2 3))',
        'and so does a List';
}

# --- what must keep working -------------------------------------------------

{
    my $s;
    $s<k> = 5;
    is-deeply $s, {k => 5}, 'an UNDEFINED scalar still autovivifies to a Hash';
}

{
    my $s = Any;
    $s<k> = 5;
    is $s<k>, 5, 'a type object still autovivifies';
}

{
    my $s;
    $s[0] = 5;
    is-deeply $s, $[5], 'the positional twin still autovivifies to an Array';
}

{
    my $s = {a => 1};
    $s<k> = 5;
    is $s<k>, 5, 'a Hash still stores';
    is $s<a>, 1, 'without disturbing what was there';
}

{
    my $s = [1, 2, 3];
    $s[0] = 9;
    is $s[0], 9, 'an Array still stores positionally';
}

{
    my $s = Buf.new(1, 2, 3);
    $s[0] = 9;
    is $s[0], 9, 'a Buf still writes a byte positionally';
}

{
    my class AssocThing does Associative {
        has %.h;
        method AT-KEY($k) is rw { %!h{$k} }
    }
    my $s = AssocThing.new;
    $s<k> = 5;
    is $s<k>, 5, 'a user class doing Associative still dispatches AT-KEY';
}

{
    my %h;
    %h<a><b> = 1;
    is %h<a><b>, 1, 'a %-sigiled name is untouched by the scalar rule';
}

{
    my @a;
    @a[0] = 5;
    is @a[0], 5, 'and so is an @-sigiled one';
}
