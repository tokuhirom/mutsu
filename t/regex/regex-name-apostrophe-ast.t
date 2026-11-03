use v6;
use Test;
use MONKEY-SEE-NO-EVAL;

plan 6;

# An apostrophe between identifier characters joins the parts of a rule name
# (`with'hyphen`); the regex tree parser used to read it as the start of a
# quote, so the literal had no source tree and `.AST` of the unit failed.

my regex with'hyphen { a }
ok 'a' ~~ /<with'hyphen>/, 'a rule named with an apostrophe is called by name';
is (~('xay' ~~ /<with'hyphen>/)), 'a', 'and matches mid-string';

my $unit = Q[my regex with'hyphen { a }; ~('a' ~~ /<with'hyphen>/)];
lives-ok { $unit.AST }, '.AST of a unit calling such a rule succeeds';
is EVAL($unit.AST), 'a', 'EVAL of that tree matches';

is EVAL(Q[my regex it's { b }; ~('b' ~~ /<it's>/)].AST), 'b', 'a trailing-s name';
is (~('a' ~~ /<[a]> ' ' | 'a'/)), 'a', 'a quote after a non-name character still opens a quote';
