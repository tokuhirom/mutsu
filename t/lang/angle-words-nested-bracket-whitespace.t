use Test;

plan 7;

# Inside a nested `<...>` pair of a `<...>` quote-word list, whitespace right
# after the inner `<` and right before the inner `>` is dropped.
is-deeply <a < b > c>, ("a", "<b>", "c"), 'padded nested pair is one word';
is-deeply <a < > c>, ("a", "<>", "c"), 'empty padded nested pair';
is-deeply <a <> c>, ("a", "<>", "c"), 'empty nested pair';
is-deeply <a < b c > d>, ("a", "<b", "c>", "d"), 'inner whitespace still separates words';
is-deeply <a <b > c>, ("a", "<b>", "c"), 'only trailing padding';
is-deeply <a < b> c>, ("a", "<b>", "c"), 'only leading padding';
my @got = do for <& < > '> -> $c { $c };
is-deeply @got, ["&", "<>", "'"], 'the loop over <& < > \'> sees three words';
