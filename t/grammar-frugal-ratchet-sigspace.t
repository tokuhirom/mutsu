# Frugal quantifiers under ratchet (token/rule) must grow shortest-first until
# the rest of the pattern matches — raku grows `\S+?` inside a `token`. Plus a
# `rule`'s implicit sigspace must insert `<.ws>` before a `$<name>=` capture
# alias (the space there is significant), not treat the `$` as an end anchor.
use Test;
plan 9;

# frugal quantifier in a ratcheted token grows past the minimum
grammar Frugal {
    token a { \S+? ',' }
    token b { .+? ',' }
    token c { $<to>=\S+? ',' }
}
ok Frugal.parse("perl6,", :rule<a>), 'ratcheted \S+? grows to reach the comma';
ok Frugal.parse("perl6,", :rule<b>), 'ratcheted .+? grows to reach the comma';
is ~(Frugal.parse("perl6,", :rule<c>)<to>), 'perl6',
    'named-capture frugal grows and captures the whole run';

# greedy under ratchet still commits to the longest (and thus fails here)
grammar Greedy { token a { \S+ ',' } }
nok Greedy.parse("perl6,", :rule<a>), 'ratcheted \S+ commits to longest, no backtrack';

# a `rule`'s sigspace must put <.ws> before a $<name>= capture
grammar Sig {
    rule greet { [Hi|Hey|Yo] $<to>=\S+? ',' }
    rule two   { xx $<t>=yy }
}
my $g = Sig.parse("Hi perl6,", :rule<greet>);
ok $g, 'rule with leading alternation + named frugal capture parses';
is ~$g<to>, 'perl6', 'greet captures the recipient';
ok Sig.parse("xx yy", :rule<two>), 'rule inserts <.ws> before $<name>= (space is significant)';

# the end-of-string anchor `$` must still suppress <.ws> (no regression)
grammar Anchor { rule tail { foo $ } }
ok Anchor.parse("foo", :rule<tail>), 'trailing $ anchor still works under sigspace';
nok Anchor.parse("foo bar", :rule<tail>), 'trailing $ anchor rejects extra input';
