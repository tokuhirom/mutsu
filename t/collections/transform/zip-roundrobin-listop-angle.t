use Test;

# `zip` and `roundrobin` are list-prefix routines, so an unparenthesized
# `<...>` word list after them is a quote-word argument, not a `<` comparison.
# `roundrobin`'s `:slip` adverb flattens the tuples into one Seq.

plan 8;

is (zip <a b c>, <d e f>, <g h i>).map(*.join(",")).join("|"), "a,d,g|b,e,h|c,f,i",
    'zip with unparenthesized <...> word-list args';

is (zip <1 2 3>, [1, 2, 3], (1, 2, 3), :with(&infix:<*>)).join("|"), "1|8|27",
    'zip with :with and mixed unparenthesized args';

is-deeply (roundrobin <a b c>, <d e f>, <g h i>).List,
    (("a", "d", "g"), ("b", "e", "h"), ("c", "f", "i")),
    'roundrobin with unparenthesized <...> word-list args';

is-deeply (roundrobin <a b c>, <d e f m n o p>, <g h i j>).List,
    (("a","d","g"), ("b","e","h"), ("c","f","i"), ("m","j"), ("n",), ("o",), ("p",)),
    'roundrobin with ragged streams';

is-deeply (roundrobin <a b c>, <d e f m n o p>, <g h i j>, :slip).List,
    ("a","d","g","b","e","h","c","f","i","m","j","n","o","p"),
    'roundrobin :slip flattens the tuples';

# parenthesized forms still work
is-deeply zip(<a b>, <c d>).List, (("a","c"), ("b","d")), 'zip(...) parenthesized still works';
is-deeply roundrobin(<a b>, <c d>).List, (("a","c"), ("b","d")), 'roundrobin(...) parenthesized still works';

# `zip` still usable inside `for`
my @out;
@out.push(.join(",")) for zip <a b c>, <d e f>;
is @out.join("|"), "a,d|b,e|c,f", 'zip in a for loop';
