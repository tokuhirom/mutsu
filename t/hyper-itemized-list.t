use Test;

# A hyper walks the *node's own* elements. Itemization is a property of the
# container a value sits in, not of the list `>>` is walking, so `>>` descends
# straight into an itemized list — unlike list-assignment flattening, where an
# itemized list is deliberately one element.
#
# Reading it as one element wrapped every result in a one-element list, and
# every downstream operation then saw one thing: `$installed>>.key.sort` in
# `DBIish`'s `t/01-basic` came out unsorted because `.sort` on a one-element
# list is a no-op.

plan 9;

my $pairs = $(:a(1), :b(2), :c(3));
is-deeply ($pairs>>.key).List, ("a", "b", "c"),
    'a hyper descends into an itemized list';
is-deeply ($pairs>>.key.sort).List, ("a", "b", "c"),
    'so a method chained onto the result sees the elements';

# Flattening semantics are unchanged: they answer a different question.
is (my @flat = $pairs).elems, 1, 'list assignment still sees one element';
is $pairs.elems, 2 + 1, 'and .elems still reports the list length';

# The elements keep their own itemization — only the top level is descended.
my $nested = $($(1, 2), $(3, 4));
is-deeply ($nested>>.Str).raku, '($("1", "2"), $("3", "4"))',
    'nested itemization survives, one level down';

# A non-list itemized value is a single element, in raku too.
is-deeply ($(5)>>.Str).List, ("5",), 'an itemized scalar stays one element';

# A plain list is untouched.
is-deeply ((1, 2)>>.Str).List, ("1", "2"), 'a plain list is unaffected';
is-deeply ((1, 2)>>.Array).raku, '([1], [2])',
    'and a leaf result is still not itemized';

# A hash hypers over its values and keeps its keys.
my %h = a => 1, b => 2;
is-deeply (%h>>.Str), {a => "1", b => "2"}, 'a Hash hypers over its values';
