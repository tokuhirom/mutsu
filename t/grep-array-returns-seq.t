use Test;

plan 9;

# `.grep` always returns a Seq in Raku, including when called on an Array (map
# already did; grep over an Array used to return a List). The Seq still carries
# the matched slots as writable containers, so an in-place update loop mutates
# the source array through them.

my @a = 1, 2, 3;
is @a.grep(* > 1).WHAT.^name, 'Seq', 'grep over an Array returns a Seq';
is @a.grep(* > 1).raku, '(2, 3).Seq', 'grep Seq .raku shows .Seq';
is @a.grep(* > 1).elems, 2, 'grep Seq elems';

# grep over a bare list is (still) a Seq
is (1, 2, 3).grep(* > 1).WHAT.^name, 'Seq', 'grep over a List returns a Seq';

# writeback through the grep result still works
{
    my @b = 1, 2, 3;
    for @b.grep(* > 1) { $_++ }
    is-deeply @b, [1, 3, 4], 'for over grep result writes back through the source';
}
{
    my @b = 1, 2, 3;
    @b.grep(* > 1)>>++;
    is-deeply @b, [1, 3, 4], 'hyper-increment over grep result writes back';
}

# a named copy of a grep result owns its values (no writeback)
{
    my @b = 1, 2, 3;
    my @g = @b.grep(* > 1);
    @g[0]++;
    is-deeply @b, [1, 2, 3], 'a copied grep result does not write back';
    is-deeply @g, [3, 3], 'the copy was mutated independently';
}

# adverbs still work and produce a Seq
is @a.grep(* > 1, :k).raku, '(1, 2).Seq', 'grep :k adverb returns a Seq of keys';
