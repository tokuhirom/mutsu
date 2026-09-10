use Test;

# Nodality decides two things about `>>.method`: whether the hyper descends into
# an Iterable element, and whether the result is a List (node level) or an Array
# (descended, container type preserved). mutsu used to mark the *coercers and
# introspectors* nodal, so `@a>>.Str` stopped at the node and yielded a List
# where Rakudo descends to the leaves and yields an Array.

plan 24;

my @flat = 1, 2, 3;
my @nested = (1, 2), (3,);

# --- non-nodal: coercers descend and preserve the Array container ------------

is @flat>>.Str.WHAT.gist,  '(Array)', '>>.Str over an Array yields an Array';
is @flat>>.Int.WHAT.gist,  '(Array)', '>>.Int over an Array yields an Array';
is @flat>>.Bool.WHAT.gist, '(Array)', '>>.Bool over an Array yields an Array';
is @flat>>.gist.WHAT.gist, '(Array)', '>>.gist over an Array yields an Array';
is @flat>>.raku.WHAT.gist, '(Array)', '>>.raku over an Array yields an Array';
is @flat>>.defined.WHAT.gist, '(Array)', '>>.defined over an Array yields an Array';

is @nested>>.Str.raku, '[("1", "2"), ("3",)]',
    '>>.Str descends into nested Iterables instead of stringifying each node';
is @nested>>.Int.raku, '[(1, 2), (3,)]',
    '>>.Int descends into nested Iterables';
is @nested>>.succ.raku, '[(2, 3), (4,)]',
    'an ordinary method still descends (unchanged)';

# A List target keeps yielding a List -- only the Array case was wrong.
is (1, 2)>>.Str.WHAT.gist, '(List)', '>>.Str over a List still yields a List';

# --- nodal: applied at the node, result is a List ----------------------------

is @nested>>.elems.raku, '(2, 1)', '.elems is nodal: it counts each node';
is @nested>>.elems.WHAT.gist, '(List)', 'a nodal method yields a List even over an Array';
is @nested>>.reverse.raku, '((2, 1).Seq, (3,).Seq)', '.reverse is nodal';
is @nested>>.join(",").raku, '("1,2", "3")', '.join is nodal';
is @flat>>.elems.raku, '(1, 1, 1)', '.elems at the node level over scalar elements';

# --- the hyper still writes back / composes ---------------------------------

is (@flat >>+>> 10).raku, '[11, 12, 13]', 'a hyper operator over an Array still yields an Array';

# --- the metaobject introspectors are not hyper-dispatched at all ------------
# Rakudo compiles .WHAT/.WHO/.HOW/.DEFINITE as special forms, so `>>` never
# reaches the elements: they apply to the target itself.

is (@nested>>.WHAT).raku, 'Array', '>>.WHAT applies to the target, not the elements';
is (@nested>>.DEFINITE).raku, 'Bool::True', '>>.DEFINITE applies to the target';
# (mutsu renders a Stash as a plain Hash -- compare against the target's own
# .WHO instead of pinning the type name.)
is (@nested>>.WHO).raku, @nested.WHO.raku, '>>.WHO is the target package stash';
is ((1, 2)>>.WHAT).raku, 'List', '>>.WHAT over a List is the List type object';
is (%(:a(1))>>.WHAT).raku, 'Hash', '>>.WHAT over a Hash is the Hash type object';
is (Int>>.DEFINITE).raku, 'Bool::False', '>>.DEFINITE over a type object is False';

# A *quoted* method name is a genuine dynamic method call and does hyper.
is (@flat>>."WHAT"()).raku, '[Int, Int, Int]', 'a quoted >>."WHAT"() still hypers';

# .WHICH is an ordinary method in Rakudo, so it keeps hypering.
is (@flat>>.WHICH).elems, 3, '>>.WHICH still hypers';

done-testing;
