use Test;

plan 12;

# `values` is `self.list`: a List, whatever the receiver
is (1,2,3).Seq.values.WHAT.raku, 'List', 'Seq.values is a List';
is (1,2,3).map(* + 1).values.WHAT.raku, 'List', 'finite map pipe .values is a List';
is gather { take 1; take 2 }.grep(* > 0).values.raku, '(1, 2)', 'gather grep .values.raku';

# a `$`-held Seq over a gather pipe renders itemized, like any other Seq
{
    my $a = gather { take 1; take 2 }.grep(* > 0);
    is $a.raku, '$((1, 2).Seq)', 'itemized gather grep Seq';
    my $b = gather { take 1; take 2 }.map(* + 1);
    is $b.raku, '$((2, 3).Seq)', 'itemized gather map Seq';
    is $b.elems, 2, 'still readable after .raku';
}

# a lazy List view is not a Seq
is (lazy 1,2,3).List.raku, '(1, 2, 3).lazy', 'lazy .List.raku';
is (lazy 1,2,3).list.raku, '(1, 2, 3).lazy', 'lazy .list.raku';
is (lazy 1,2,3).cache.raku, '(1, 2, 3).lazy', 'lazy .cache.raku';
is (lazy gather { take 1; take 2 }).map(* + 1).List.raku, '(2, 3).lazy', 'lazy pipe .List.raku';
is (lazy 1,2,3).Seq.raku, '(1, 2, 3).lazy.Seq', 'lazy Seq keeps .Seq';
is (1..*).map(* + 0).List.raku.ends-with('...).lazy'), True, 'infinite pipe List view ends with .lazy';
