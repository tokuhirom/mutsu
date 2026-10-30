use lib 't/lib';
use Test;
use MonkeyClosureRegex;

plan 3;

# The module that built the regex says `use MONKEY-SEE-NO-EVAL`; this file
# does not. The pragma is lexical, so it travels with the regex, and the code
# block of the interpolated string sees the lexicals where the regex was written.
my $r = MonkeyClosureRegex::Router.new;
my $re = make-regex($r, '[ "ab" { $note.note($/) } ]');

ok "ab" ~~ $re, 'a regex built under MONKEY-SEE-NO-EVAL matches from a file without the pragma';
is $r.seen.elems, 1, 'the code block ran';
is $r.seen[0], 'ab', 'it saw the defining scope lexical and the match';
