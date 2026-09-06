use Test;

# Two independent defects, both on the way from a `key => value` Pair into a
# key-constrained (object) hash. Every expectation was measured against raku
# v2026.07 first.
#
# 1. Rakudo's `infix:<< => >>` binds the key as a plain `Mu $key` and the value
#    as `Mu \value`, so only the VALUE keeps a container. mutsu handed the key
#    through whole, so an itemized key stayed itemized.
#
# 2. Assigning a hash whose values are `:=`-bound cells rebuilt it as a bare
#    map, dropping `original_keys` -- and with it the object-hash keys. So the
#    SAME assignment succeeded with a literal value and died with
#    "expected List:D but got Str" when the value came from a container read.

plan 21;

# --- 1. the key is decontainerized, the value is not ----------------------

{
    my $s = $(1, 2);
    is ($s => "x").raku, '(1, 2) => "x"', 'an itemized key is decontainerized';
    is ($s => "x").key.raku, '(1, 2)', '... so .key reports the bare list';
    is ($s => "x").key.VAR.^name, 'List', '... and has no container of its own';

    my $i = $[3, 4];
    is ($i => "x").key.raku, '[3, 4]', 'an itemized Array key, too';

    my @a = 1, 2;
    is (@a => "x").key.raku, '[1, 2]', 'a plain Array key is unchanged';
    is ((1, 2) => "x").key.raku, '(1, 2)', 'a literal list key is unchanged';
    my $n = 5;
    is ($n => "x").key.raku, '5', 'a plain scalar key is unchanged';
    is ($n => "x").key.VAR.^name, 'Int', '... and still has no container';
}

# The VALUE keeps its container: that is what makes `$pair.value = X` write
# through, and an itemized value stays itemized.
{
    my $v = 7;
    # NB read through a variable: passing the expression straight to `is`
    # decontainerizes it on the way into the argument, in mutsu today.
    my $vname = ("k" => $v).value.VAR.^name;
    is $vname, 'Scalar', 'the value keeps its container';
    my $p = ("k" => $v);
    $p.value = 9;
    is $v, 9, '... so assigning through the pair writes back';

    my $t = $(8, 9);
    is ("k" => $t).raku, ':k($(8, 9))', 'an itemized value stays itemized';
    is ("k" => $t).value.raku, '$(8, 9)', '... on the value side';
}

# --- 2. the object-hash key survives a container-read value ---------------

{
    my %h = :path($(1, 2)), :value(9);

    # A literal value always worked; the container-read value is the fix.
    for (
        (%h<path> => %h<value>),
        (%h<path> => 9),
        ($(1, 2) => %h<value>),
        ($(1, 2) => 9),
    ) -> $p {
        my Any:D %t{List:D} = ($p,);
        is %t.raku, '(my Any:D %{List:D} = (1, 2) => 9)',
           "the List:D key survives: {$p.raku}";
    }
}

# The other HashData metadata on the same cliff.
{
    my %src = :v(5);
    my Int %typed = (a => %src<v>);
    is %typed.raku, '(my Int % = :a(5))', 'the value-type constraint survives';
    is %typed<a>, 5, '... and the entry reads back';
}
{
    my %src = :v(5);
    my %d is default('N/A') = (a => %src<v>);
    is %d<a>, 5, 'a present key reads its value';
    is %d<nope>, 'N/A', 'the `is default` value survives the assignment';
}

# A plain (Str-keyed) hash is unchanged by either fix.
{
    my %src = :v(5);
    my %plain = (a => %src<v>, b => 6);
    is %plain.raku, '{:a(5), :b(6)}', 'a plain hash assignment is unchanged';
}
