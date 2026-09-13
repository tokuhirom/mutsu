use v6;
use Test;

# The scanner that finds a capture group's closing `)` has to read `<...>` by
# the rule that applies to it, and the two rules are opposites:
#
#   * a character CLASS (`<[...]>`, `<-[...]>`, `<+[...]>`, `<:Prop>`) holds
#     literal members, so a `)` or a quote inside it is just a member;
#   * an ASSERTION or subrule call (`<!before ...>`, `<?{...}>`, `<foo>`) holds a
#     nested regex, so a quote inside it really does open a string and a `>`
#     inside that string does NOT close the assertion.
#
# Keying both off one `<...>` nesting depth can only get one of the two right:
# `( \d+ <[.)]> )` ended at the class's `)` and died on the leftover `]`
# (Markdown::Lex, Blogin), while `( [ <!before '>}}'> . ]* )` ended at the `>`
# inside the assertion's string (Blogin). Refs #7954.
plan 11;

# --- a paren inside a character class is a member ---------------------------

ok 'a)' ~~ / ( <[)]> ) /, 'a bare `)` char class inside a capture group';
is $0, ')', '... and it captures the paren';

ok '1)' ~~ / ( \d+ <[.)]> ) /, 'Markdown::Lex ordered-list marker';
is $0, '1)', '... captures digits plus terminator';

ok 'a(' ~~ / ( <[(]> ) /, 'an unbalanced `(` char class does not open a group';

my $m = '- x' ~~ / ^ $<marker> = ( <[-+*]> | \d+ <[.)]> ) /;
is $m<marker>, '-', 'Blogin bullet-marker alternation, named capture intact';
is ('12. y' ~~ / ^ $<marker> = ( <[-+*]> | \d+ <[.)]> ) /)<marker>, '12.',
    '... and its other branch';

# --- a quote inside an ASSERTION is a string, and a `>` in it is literal -----

ok 'x>y' ~~ / ( [ <!before '>'> . ]* ) /, 'a quoted `>` inside <!before> stays inside it';
is $0, 'x', '... so the assertion stops the repetition at the `>`';

# --- ...but a quote inside a character CLASS is a member --------------------

ok Q{a'b} ~~ / ( <-['"]>+ ) /, 'quotes are literal members of a char class';
is $0, 'a', '... and the class still excludes them';

done-testing;
