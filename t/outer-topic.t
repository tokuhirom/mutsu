use Test;

plan 7;

# $OUTER::_ in a stored closure accesses the enclosing topic even after the
# defining block has exited.
# https://github.com/Raku/old-issue-tracker/issues/1665
{
    my $t;
    for 'a' {
        $t = sub { $OUTER::_ };
    }
    is $t(), 'a', '$OUTER::_ can access a $_';
}

# $OUTER::x resolves a named enclosing lexical from a stored closure.
{
    my $u;
    for 'b' {
        my $x = 'inner-b';
        $u = sub { $OUTER::x };
    }
    is $u(), 'inner-b', '$OUTER::x reaches the enclosing named lexical';
}

# Inline nested blocks still resolve OUTER by lexical depth (unchanged path).
{ my $a = 1; {
   my $a = 2; {
      my $a = 3;
      is $a,               3, 'get regular $a';
      is $OUTER::a,        2, 'get $OUTER::a';
      is $OUTER::OUTER::a, 1, 'get $OUTER::OUTER::a';
}}}

# A closure that also uses its own topic keeps them distinct: the sub's own
# `$_` is a fresh (undefined) topic, while `$OUTER::_` is the enclosing one.
{
    my $c;
    for 'outer' {
        $c = sub { $_.defined ?? "own-defined" !! "own={$OUTER::_}" };
    }
    is $c(), 'own=outer', 'own $_ and $OUTER::_ stay distinct';
}

# The captured OUTER binding survives multiple later invocations.
{
    my $g;
    for 'z' {
        $g = sub { $OUTER::_ };
    }
    is $g() ~ $g(), 'zz', 'repeated invocations keep the captured topic';
}
