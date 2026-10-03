use Test;

# `<|b>` / `<|c>` / `<|g>` hold at every match position, and `<!|w>` is "not
# at a word boundary". From Badger's grammar
# `$<type>=<.qualified-name> <|b> \s* ...`.

plan 6;

is-deeply "ab cd".match(/<|b>/, :g).map(*.from).List, (0, 1, 2, 3, 4, 5), '<|b> everywhere';
is-deeply "ab cd".match(/<|c>/, :g).map(*.from).List, (0, 1, 2, 3, 4, 5), '<|c> everywhere';
is-deeply "ab cd".match(/<|g>/, :g).map(*.from).List, (0, 1, 2, 3, 4, 5), '<|g> everywhere';
is-deeply "ab cd".match(/<!|w>/, :g).map(*.from).List, (1, 4), '<!|w> is not-a-word-boundary';
nok "ab cd" ~~ /<!|b>/, '<!|b> never matches';
is ~("Result2 \$" ~~ /\w+ <|b> \s* '$'/), 'Result2 $', '<|b> between a word and spaces';
