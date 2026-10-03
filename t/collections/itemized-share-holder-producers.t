use Test;

# ADR-0079 slice 3, producer audit: every store that hands an aggregate to a
# `Scalar` holder itemizes it, and a whole reassignment of an `=`-share holder
# replaces that holder without writing through the cell it shares with the
# source. Every expectation was measured on rakudo.

plan 24;

# A list assignment (and an assignment in expression position) to a `$`
# holder of a share replaced the SOURCE's contents too: the env mirror still
# named the shared cell, and the by-name write stored through it.
{
    my %h = a => 1; my $x = %h; my $y = 5;
    ($x, $y) = 7, 8;
    is %h.raku, '{:a(1)}', 'list assignment to a share holder leaves the source alone';
    is $x, 7, 'the holder took the new value';
}
{
    my %h = a => 1; my @r = 1, 2;
    my $x = %h; my $y = @r;
    ($x, $y) = ($y, $x);
    is-deeply (%h.raku, @r.raku), ('{:a(1)}', '[1, 2]'), 'swapping two share holders keeps both sources';
    is-deeply ($x.raku, $y.raku), ('$[1, 2]', '${:a(1)}'), 'the holders swapped';
}
{
    my %h = a => 1; my $x = %h;
    my $r = ($x = 5);
    is-deeply (%h.raku, $x, $r), ('{:a(1)}', 5, 5), 'an expression-position reassignment replaces the holder';
}
{
    my @r = 1, 2; my $x = @r;
    $x = 5; $x++;
    is-deeply (@r.raku, $x), ('[1, 2]', 6), 'the replaced holder is an ordinary scalar afterwards';
}

# A sigilless declaration binds the aggregate itself; it is not a `Scalar`.
{
    # (Distinct names: a sigilless name leaks into later same-named `$`
    # declarations at compile time -- see the PR's follow-up issue.)
    my %h = a => 1; my \sx = %h;
    is sx.raku, '{:a(1)}', 'a sigilless binding of a hash is not itemized';
    %h<b> = 2;
    is sx.raku, '{:a(1), :b(2)}', 'it still aliases the source';
    my %c = (sx,);
    is %c.raku, '{:a(1), :b(2)}', 'it flattens in a hash initializer';
    my @r = 1, 2; my \sy = @r;
    is sy.raku, '[1, 2]', 'a sigilless binding of an array is not itemized';
}

# `state $x = AGGREGATE` is a `Scalar` holder like `my $x = AGGREGATE`.
{
    my %h = a => 1; my @r = 1, 2;
    state $s = %h; state $t = @r;
    is $s.raku, '${:a(1)}', 'a state scalar initialized from a hash is itemized';
    is $t.raku, '$[1, 2]', 'a state scalar initialized from an array is itemized';
    throws-like { my %c = ($s,) }, X::Hash::Store::OddNumber,
        'the state holder stays one opaque hash-initializer item';
}

# A chained share (`$y = $x`) keeps the source scalar's own itemization.
{
    my @r = 1, 2; my $x = @r; my $y = $x;
    is $x.raku, '$[1, 2]', 'the chained source stays itemized';
    is $y.raku, '$[1, 2]', 'the chained target is itemized';
    is @r.raku, '[1, 2]', 'the aggregate source stays plain';
    $x //= 5;
    is $x.raku, '$[1, 2]', '//= on a defined share holder keeps it itemized';
}

# A multi-dimensional element is a `Scalar` too.
{
    my %h = a => 1;
    my @b; @b[0;1] = %h;
    is @b[0;1].raku, '${:a(1)}', 'a multi-dim element assignment itemizes';
    throws-like { my %c = (@b[0;1],) }, X::Hash::Store::OddNumber,
        'the multi-dim element stays one opaque hash-initializer item';
    my @s[2;2]; @s[0;1] = %h;
    is @s[0;1].raku, '${:a(1)}', 'a shaped multi-dim element assignment itemizes';
}

# The atomic stores write a `Scalar` holder too.
{
    my %h = a => 1;
    my $x; atomic-assign($x, %h);
    is $x.raku, '${:a(1)}', 'atomic-assign itemizes';
    my $y; cas($y, Any, %h);
    is $y.raku, '${:a(1)}', 'cas($var, $expected, $new) itemizes';
    my $z = 1; cas($z, -> $ { [1, 2] });
    is $z.raku, '$[1, 2]', 'cas($var, &code) itemizes';
    my @a = 1; cas(@a[0], @a[0], [4]);
    is @a[0].raku, '$[4]', 'cas on an array element itemizes';
}
