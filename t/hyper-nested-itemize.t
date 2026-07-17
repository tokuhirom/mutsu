use v6;
use Test;

plan 12;

# Rakudo's `deepmap` (which non-nodal `>>.foo` builds on) recurses into an
# element that is Iterable and itemizes what comes back, but applies the method
# straight to a leaf and leaves that result alone.

is (((1,),)>>.succ).raku, '($(2,),)', 'a nested one-element list result is itemized';
is (((1,2),(3,))>>.succ).raku, '($(2, 3), $(4,))', 'every nested list result is itemized';
is ((((1,),),)>>.succ).raku, '($($(2,),),)', 'itemization applies at every depth';
is (((1,).Seq,)>>.succ).raku, '($(2,),)', 'a nested Seq result is itemized too';

# A leaf is not descended into, so a list-returning method on a leaf keeps its
# result un-itemized.
is ((1,2)>>.succ).raku, '(2, 3)', 'a flat list of leaves is not itemized';
is ((1,2)>>.Array).raku, '([1], [2])', 'a list result from a leaf method is not itemized';

# Hash values are Iterable positions as well.
is (({a => (1,2)}>>.succ)).raku, '{:a($(2, 3))}', 'a hash value list result is itemized';
is (((%(a => 1),)>>.succ)).raku, '(${:a(2)},)', 'hyper descends into a nested hash';
is ((({a=>1}, {b=>2})>>.succ)).raku, '(${:a(2)}, ${:b(3)})', 'each nested hash is descended and itemized';

# Nodal methods still apply to the container itself, not its leaves.
is (((%(a => 1),)>>.keys)).raku, '(("a",).Seq,)', 'a nodal method is not descended';
is ((((1,2),(3,))>>.List)).raku, '((1, 2), (3,))', '.List is nodal';

# An Array prints its (always-itemized) elements without the `$` sigil.
my @a = (1,), (2,3);
is (@a>>.succ).raku, '[(2,), (3, 4)]', 'an Array target keeps Array shape';
