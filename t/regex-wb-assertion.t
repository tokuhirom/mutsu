use Test;

# `<?wb>` is a zero-width assertion at a word boundary (a transition between a
# word char and a non-word char, either direction — i.e. `<<` or `>>`), and
# `<!wb>` asserts NOT at a boundary. (Language/regexes.rakudoc.)

plan 10;

ok "two-words" ~~ / two<?wb>\-<?wb>words /, '<?wb> around a hyphen';
ok "twowords"  ~~ / two<!wb><!wb>words /,    '<!wb> inside a word';
nok "two words" ~~ / two<!wb>\s words /,     '<!wb> fails at a real boundary';

# string edges are boundaries
ok "cat" ~~ /<?wb>cat<?wb>/,  '<?wb> at both string edges';
ok "cat" ~~ /cat<?wb>/,       '<?wb> at end of string';
ok "cat" ~~ /<?wb>cat/,       '<?wb> at start of string';

# <!wb> inside a word matches, at a boundary fails
ok  "abcd"  ~~ /ab<!wb>cd/,   '<!wb> between two word chars';
nok "ab cd" ~~ /ab<!wb> cd/,  '<!wb> before a space fails';

# substitution inserts at every boundary
is "stuff here!!!".subst(:g, /<?wb>/, "|"), "|stuff| |here|!!!", '<?wb> subst marks all boundaries';
is "a-b".subst(:g, /<?wb>/, "|"),           "|a|-|b|",           '<?wb> subst around punctuation';
