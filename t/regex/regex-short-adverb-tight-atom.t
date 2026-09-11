use v6;
use Test;

# A short inline regex adverb (`:i`, `:s`, `:r`, `:m`) ends wherever an
# identifier would: the atom it scopes over may follow immediately, with no
# space. mutsu required a space, a `:` or a `/` after it, so `/:i'not('/ —
# how CSS::Grammar spells every case-insensitive CSS keyword — left the adverb
# unrecognized and the whole pattern failed to match.

plan 11;

ok 'abc' ~~ /:i'abc'/,   ":i'...' with no space";
ok 'ABC' ~~ /:i'abc'/,   ":i'...' still ignores case";
ok 'not(' ~~ /:i'not('/, 'a literal containing a metacharacter';
ok 'a[b' ~~ /:i'a[b'/,   'a literal containing a bracket';
ok 'a c' ~~ /:i 'a c'/,  'the spaced spelling still works';

ok "x\ny" ~~ /:i"x\ny"/, 'a newline between the adverb and its atom is fine';
ok 'AB' ~~ /:i<[ab]>+/,  ':i before a character class';

# Longer adverbs must NOT be mistaken for a short one plus text.
grammar G {
    rule  r { :my $*seen = 1; 'a' { $*seen = 2 } 'b' }
    proto token p {*}
    token p:sym<ss> { 'ss' }
    token ratchety { :r 'ab' }
}
ok G.parse('a b', :rule<r>).defined, ':my is not read as :m + "y"';
ok G.parse('ss', :rule<p>).defined,  ':sym<...> is not read as :s + "ym..."';
ok G.parse('ab', :rule<ratchety>).defined, ':r still parses';

nok 'abc' ~~ /:i'xyz'/, 'a non-matching tight literal still fails';
