use Test;

plan 61;

# ---------------------------------------------------------------------------
# <same> / <wb> / <ww> -- bare builtin zero-width assertions.
#
# <same> succeeds when the character BEFORE the current position is equal to
# the character AFTER it, so it needs a character on both sides: it fails at
# the start and at the end of the string. Like any other bare subrule call it
# also publishes a (zero-width) named capture; the `<.name>` spelling does not.
# ---------------------------------------------------------------------------

is ~('123345' ~~ m/ <same>\d+ /), '345', '<same> anchors between the doubled 3s';
nok 'aa11' ~~ m/ <alpha><same><digit> /, '<same> is not a backreference';
is ~('aa' ~~ m/ . <same> /), 'a', '<same> after a consumed char';
nok 'aa' ~~ m/^ <same> a /, '<same> fails at the start of the string';
nok 'aa' ~~ m/ 'aa' <same> /, '<same> fails at the end of the string';
nok 'aba' ~~ m/ b <same> /, '<same> compares the two adjacent chars only';

my $same-match = 'aa' ~~ m/ . <same> /;
ok $same-match<same>.defined, '<same> publishes a named capture';
is $same-match<same>.Str, '', '... which is zero-width';
is $same-match<same>.from, 1, '... at the assertion position';
nok ('aa' ~~ m/ . <.same> /)<same>.defined, '<.same> suppresses the capture';

is ~('aa' ~~ m/ . <?same> /), 'a', '<?same> still works';
is ~('ab' ~~ m/ . <!same> /), 'a', '<!same> still works';

is ~('a b' ~~ m/ a <wb> /), 'a', '<wb> matches at a word boundary';
ok ('a b' ~~ m/ a <wb> /)<wb>.defined, '<wb> publishes a named capture';
nok 'ab' ~~ m/ a <wb> b /, '<wb> fails inside a word';
is ~('ab' ~~ m/ a <ww> b /), 'ab', '<ww> matches inside a word';
ok ('ab' ~~ m/ a <ww> b /)<ww>.defined, '<ww> publishes a named capture';

# ---------------------------------------------------------------------------
# <~~> -- recursive self-match into the enclosing regex / rule.
# ---------------------------------------------------------------------------

my $paren = rx/ '(' <-[()]>* ')' || '(' [ <-[()]>* <~~> <-[()]>* ]* ')' /;
is ~('(1 + (2 x 3)) = 7' ~~ $paren), '(1 + (2 x 3))',
    '<~~> yields the OUTERMOST balanced span';
is ~('((5 + 2) x 6) = 42 (the answer)' ~~ $paren), '((5 + 2) x 6)',
    '<~~> with the nesting at the front';
is ~('(((a)))' ~~ $paren), '(((a)))', '<~~> nests three deep';
is ~('a (1 (2 (3) 4) 5) b' ~~ $paren), '(1 (2 (3) 4) 5)',
    '<~~> with siblings around the nested group';
nok ')(' ~~ $paren, 'unbalanced parens do not match';
nok 'xy' ~~ / x <~~> y /, 'a <~~> that cannot terminate simply fails';
# NOTE: a directly left-recursive `<~~>` (`/ <~~> a /`) makes Rakudo itself run
# out of stack mid-file, so it cannot be asserted here against both
# implementations. mutsu cuts such a recursion off (re-entering the same source
# at the same position fails), and the `xy` case above covers the terminating
# path that both implementations agree on.

grammar BalancedBrackets {
    token TOP { '[' [ <-[\[\]]>* <~~> <-[\[\]]>* ]* ']' || '[' <-[\[\]]>* ']' }
}
is ~BalancedBrackets.parse('[a[b]c]'), '[a[b]c]', '<~~> recurses into a grammar token';

# ---------------------------------------------------------------------------
# <!:Prop> / <?:Prop> -- zero-width, but they still need a character.
#
# Rakudo bounds-checks the character-class matcher BEFORE applying the
# negation, so both polarities fail at end of string; the greedy quantifier in
# front has to hand a character back. (An enumerated `<![...]>` / `<!+name>`
# class is an ordinary lookaround and DOES succeed there.)
# ---------------------------------------------------------------------------

is ~('333' ~~ m/^^ \d+ <!:L> /), '33', '<!:L> needs a character to look at';
is ~('333' ~~ m/^^ \d+ <!:Script<Tamil>> /), '33', '... and so does <!:Script<Name>>';
is ~('333' ~~ m/^^ \d+ <!:Script<Latin>> /), '33', '... whichever script it names';
is ~('33a' ~~ m/^^ \d+ <!:L> /), '3', '<!:L> is zero-width, not consuming';
is ~('33a' ~~ m/^^ \d+ <-:L> /), '33', '<-:L> by contrast consumes';
nok '333' ~~ m/^^ \d+ <!:Nd> /, '<!:Nd> never succeeds among digits';
nok '' ~~ m/^ <!:L> /, '<!:L> fails on an empty string';
is ~(' ' ~~ m/^ <!:L> /), '', '<!:L> matches zero-width before a space';
is ~('1b' ~~ m/^ <?:!L> /), '', '<?:!L> asserts the negated property';
nok '333' ~~ m/^^ \d+ <!:!L> /, '<!:!L> asserts the property itself';
is ~('333' ~~ m/^^ \d+ <![abc]> /), '333', '<![abc]> succeeds at end of string';
is ~('333' ~~ m/^^ \d+ <!+alpha> /), '333', '<!+alpha> succeeds at end of string';
is ~('333' ~~ m/^^ \d+ <!alpha> /), '333', '<!alpha> succeeds at end of string';

# ---------------------------------------------------------------------------
# Character-class arithmetic with a `.` (any character) base.
#
# Raku's class arithmetic is not set arithmetic: the positive and negative
# halves accumulate separately and a character matches when it is in the
# positive half and NOT in the negative half. So `.` seeds the positive half
# with every character and a later `+[1]` cannot re-admit a subtracted `a`.
# ---------------------------------------------------------------------------

is "ab1 c".comb(/<.-:letter-:digit>/).join('|'), ' ',
    'dot base with two property subtractions';
is "ab1 c".comb(/<.-[a]-[b]>/).join('|'), '1| |c',
    'dot base with two bracket subtractions';
is "ab1 c".comb(/<.-[a]-[b]-[c]>/).join('|'), '1| ',
    'dot base with three bracket subtractions';
is "ab1 ".comb(/<.-:letter>/).join('|'), '1| ',
    'dot base with one subtraction still works';
is "ab1 c".comb(/<.-[a]+[1]>/).join('|'), 'b|1| |c',
    'a later union does not re-admit a subtracted character';
is "ab1 c".comb(/<.-:letter-[\ ]>/).join('|'), '1',
    'dot base mixing a property and a bracket subtraction';
is "ab1 c".comb(/<.-:letter-[ ]>/).join('|'), '1| ',
    '... an unescaped space in the bracket is still insignificant';
is "ab1 c".comb(/<-[a]-[b]>/).join('|'), '1| |c',
    'plain chained subtraction is unaffected';

# ---------------------------------------------------------------------------
# :st / :nd / :rd / :th are exact aliases of :nth.
# ---------------------------------------------------------------------------

my $data = "f fo foo fooo foooo fooooo foooooo";
ok $data ~~ m:st(1|8)/fo+/, ':st takes a Junction of ordinals';
is ~($data ~~ m:st(2)/fo+/), 'foo', ':st(N) is :nth(N)';
is ~($data ~~ m:nd(3)/fo+/), 'fooo', ':nd(N) is :nth(N)';
is ~($data ~~ m:rd(3)/fo+/), 'fooo', ':rd(N) is :nth(N)';
is ~($data ~~ m:th(4)/fo+/), 'foooo', ':th(N) is :nth(N)';
is ($data ~~ m:st(1,3)/fo+/).map(~*).join('|'), 'fo|fooo', ':st takes a list';
is ~($data ~~ m:3rd/fo+/), 'fooo', 'the ordinal-prefix spelling still works';

# ---------------------------------------------------------------------------
# `$/` written directly before a substitution's closing delimiter.
# ---------------------------------------------------------------------------

my $s1 = 'ab'; $s1 ~~ s/(a)/[$/]/;
is $s1, '[a]b', 'a bare $/ before a bracket and the delimiter';
my $s2 = 'ab'; $s2 ~~ s:g/<[ab]>/$//;
is $s2, 'ab', 'a bare $/ as the whole replacement';
my $s3 = 'foobar'; $s3 ~~ s/foo$/X/;
is $s3, 'foobar', 'a trailing $ in the PATTERN is still the end anchor';
my $s4 = 'barfoo'; $s4 ~~ s/foo$/X/;
is $s4, 'barX', '... and still matches at the end';

# ---------------------------------------------------------------------------
# \c[...] name lookup: NameAlias corrections, named sequences, abbreviations.
# ---------------------------------------------------------------------------

is "\c[LATIN CAPITAL LETTER GHA]".ords.join('|'), '418',
    'a NameAlias correction resolves';
is "\c[LATIN CAPITAL LETTER E WITH VERTICAL LINE BELOW AND ACUTE]".ords.join('|'),
    '201|809', 'a named character sequence resolves to several codepoints';
is "\c[family: man woman girl boy]".ords.join('|'),
    '128104|8205|128105|8205|128103|8205|128102',
    'a compound CLDR emoji sequence name resolves';
is uniparse('BYTE ORDER MARK').ords.join('|'), '65279', 'the alternate alias resolves';
is uniparse('VS16').ords.join('|'), '65039', 'a variation-selector abbreviation resolves';
