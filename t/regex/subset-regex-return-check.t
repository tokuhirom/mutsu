use v6;
use Test;

plan 6;

# A subset whose `where` is a REGEX (URI's `subset Host of Str
# where /^ [ <IETF::RFC_Grammar::URI::host> ] $/`) passed `~~` but failed
# every `--> Host` return type check: the return path had its own predicate
# evaluator that only understood callable predicates, so the regex came back
# false. The return check now delegates to the same subset matcher `~~` uses.

grammar G {
    token host { \w+ [ '.' \w+ ]* }
}

subset Host of Str where /^ [ <G::host> ] $/;
subset AbLike of Str where /^ab/;

ok "raku.org" ~~ Host, 'smartmatch against the grammar-regex subset';

sub f(--> Host) { "raku.org" }
is f(), "raku.org", 'a grammar-regex subset return type accepts a match';

sub g(--> AbLike) { "abc" }
is g(), "abc", 'a plain-regex subset return type accepts a match';

sub bad(--> AbLike) { "zzz" }
throws-like { bad() }, Exception, message => /'Type check failed for return value'/,
    'a plain-regex subset return type still rejects a non-match';

# The callable-predicate path keeps working.
subset Long of Str where *.chars > 2;
sub h(--> Long) { "abc" }
is h(), "abc", 'a callable subset return type accepts';
sub bad2(--> Long) { "a" }
throws-like { bad2() }, Exception, message => /'Type check failed for return value'/,
    'a callable subset return type still rejects';
