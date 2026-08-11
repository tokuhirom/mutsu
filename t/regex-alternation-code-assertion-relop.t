use Test;

plan 4;

# A `<?{ ... }>` code assertion containing a relational operator (`<=`, `<`,
# `>=`, `>`) desynchronized `split_top_level_alternation`'s angle-bracket
# depth counter when the assertion sat inside a `[ A | B ]` alternation: the
# stray `<` from `<=` was counted as opening a NEW `<...>` assertion with no
# matching `>`, so the alternation's own `|` was never recognized as a
# top-level separator and the whole `[ ... ]` was mis-parsed as one branch
# (Cro::HTTP::Router's route matcher hits this shape for every bounds-checked
# native-int route parameter).
ok "/x" ~~ / ^ [ '/' 'x' <?{ -128 <= 1 <= 127 }> | <!> ] $ /,
    'chained comparison in code assertion inside alternation matches';

ok "/x" ~~ / ^ [ '/' 'x' <?{ 1 <= 127 }> | <!> ] $ /,
    'single <= comparison in code assertion inside alternation matches';

ok "/x" ~~ / ^ [ '/' 'x' <?{ 1 >= -128 }> | <!> ] $ /,
    'single >= comparison in code assertion inside alternation matches';

nok "/y" ~~ / ^ [ '/' 'x' <?{ 1 <= 127 }> | <!> ] $ /,
    'non-matching literal still fails via the <!> fallback branch';
