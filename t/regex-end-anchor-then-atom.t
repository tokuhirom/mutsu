use v6;
use Test;

# A `$` end-of-string anchor that is NOT the last thing in the pattern must
# still be an anchor, not a literal `$`. Only a *trailing* `$` (rest of the
# pattern is whitespace) and `$$` were handled; a bare `$` followed by any
# further atom fell through to a literal `$` in Match mode, so it demanded a
# literal `$` in the input and never matched. This blocked YAMLish's
# `Schema::Core` string fallback `token plain { ^ .* $ { make ~$<value> } }`.

plan 8;

ok ("hi" ~~ / \w+ $ <?{ True }> /).defined, 'zero-width assertion after $ matches';
ok ("hi" ~~ / \w+ $ { 1 } /).defined,        'code block after $ matches';
ok ("hi" ~~ / \w+ $ \w* /).defined,          'empty-matching atom after $ matches';
nok ("hi" ~~ / \w+ $ x /).defined,           '$ still forbids real input after end';
ok ("hi" ~~ / \w+ $$ <?{ True }> /).defined, '$$ before an atom still matches';
ok ("hi" ~~ / \w+ $ /).defined,              'trailing $ still matches';

# The motivating case: an anchored capture with a trailing action.
grammar G {
    token plain { ^ $<value>=.* $ { make ~$<value> } }
}
my $m = G.parse("hello world", :rule<plain>);
ok $m.defined, 'anchored .* with trailing make matches';
is $m.ast, "hello world", 'the action ran and captured the whole string';
