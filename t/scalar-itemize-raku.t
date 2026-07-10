use Test;

# Assignment into a `$` scalar container itemizes an Array/List value, and
# `.raku` reflects the Scalar container (rakudo checks nqp::iscont(SELF)):
#   my $x = [1,2,3];  $x.raku  # $[1, 2, 3]
# Binds (`:=`), `constant`, and sigilless declarations install the value
# itself (no Scalar container) and must NOT itemize.

plan 26;

# --- plain scalar assignment itemizes ---
{
    my $x = [1, 2, 3];
    is $x.raku, '$[1, 2, 3]', 'my $x = [...] renders $[...]';
    my $l = (1, 2, 3);
    is $l.raku, '$(1, 2, 3)', 'my $l = (...) renders $(...)';
    my $y;
    $y = [9];
    is $y.raku, '$[9]', 'plain reassignment itemizes';
    my ($a, $b) = [1, 2], (3, 4);
    is $a.raku, '$[1, 2]', 'list-assignment itemizes each scalar (array)';
    is $b.raku, '$(3, 4)', 'list-assignment itemizes each scalar (list)';
    my $r = do { my @arr = 1, 2, 3; @arr.append(4) };
    is $r.raku, '$[1, 2, 3, 4]', 'method-return stored in scalar itemizes';
}

# --- itemized scalar stays single in list context, still fully usable ---
{
    my $x = [1, 2, 3];
    my @a = 1, $x;
    is @a.elems, 2, 'itemized array stays one element in list assignment';
    is $x.elems, 3, '.elems still reaches the array';
    is $x[1], 2, 'indexing still works';
    $x.push(4);
    is $x.raku, '$[1, 2, 3, 4]', 'push mutates through the container';
}

# --- binds and constants do NOT itemize ---
{
    my $z := [5, 6];
    is $z.raku, '[5, 6]', ':= bind installs the bare value';
    constant $c = [7];
    is $c.raku, '[7]', 'constant binds the bare value';
    my \raw = [1, 2];
    is raw.raku, '[1, 2]', 'sigilless declaration binds the bare value';
    my \lol = (1, 2), (3, 4);
    is lol.raku, '((1, 2), (3, 4))', 'sigilless list declaration stays a bare List';
}

# --- @-sigil contexts strip the container ---
{
    my $x = [1, 2, 3];
    is (@$x).raku, '[1, 2, 3]', '@$x decontainerizes';
    my @b := $x;
    is @b.raku, '[1, 2, 3]', '@a := $x binds the bare array';
    my $out = '';
    for @$x { $out ~= $_ }
    is $out, '123', 'for @$x iterates elements';
}

# --- zen slice / <> decontainerize ---
{
    my $a = (1, 2, 3);
    is ($a[] X~ 'a').join(' '), '1a 2a 3a', '$a[] decontainerizes for X';
    my $x = [1, 2, 3];
    is $x[].raku, '[1, 2, 3]', '$x[] strips the container';
    is $x<>.raku, '[1, 2, 3]', '$x<> strips the container';
}

# --- hyper ops descend through the container ---
{
    my $a = (1, 2, 3);
    my $b = (2, 4, 6);
    is ($a >>+<< $b).raku, '$(3, 6, 9)', 'hyper op deconts operands, result inherits itemization';
    is ((1, 2, 3) >>+<< $b).raku, '(3, 6, 9)', 'bare left operand keeps a bare result';
    my $i = [[1, 2], [3, 4]];
    is ($i >>+>> 10).raku, '$[[11, 12], [13, 14]]', 'hyper descends nested itemized arrays';
    my $c := (1, 2, 3);
    is ($c >>[&infix:<+>]<< $c).raku, '(2, 4, 6)', 'hyper func op on bound scalars';
    is ($c >>[&infix:<+>]<< $c).raku, '(2, 4, 6)', 'writeback does not itemize a bound scalar';
    is $c.raku, '(1, 2, 3)', 'bound scalar keeps its bare List after hyper func op';
}

done-testing;
