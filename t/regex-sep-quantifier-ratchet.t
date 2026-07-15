use v6;
use Test;

plan 12;

# Ratchet (token/rule) separated quantifiers are possessive: they commit to
# the greedy chain and never backtrack into it (Rakudo semantics). The
# backtracking `regex` declarator keeps the old exploring behavior.

my token t-possessive { <[ab]>+ % ',' ',b' }
nok "a,b" ~~ &t-possessive,
    'token: sep-quantifier is possessive (cannot give back ",b")';

my regex r-backtrack { <[ab]>+ % ',' ',b' }
ok "a,b" ~~ &r-backtrack,
    'regex: sep-quantifier still backtracks (",b" is given back)';

my token t-list { <[ab]>+ % ',' }
is ("a,b,a" ~~ &t-list).Str, 'a,b,a', 'token: greedy chain consumes the whole list';

my token t-trail { <[ab]>+ %% ',' }
is ("a,b," ~~ &t-trail).Str, 'a,b,', 'token: %% consumes a trailing separator';

my token t-min { \w ** 4..* % ',' }
nok "a,b" ~~ &t-min,
    'token: unsatisfiable minimum fails (no backtracking to retry)';

my token t-zero { <[xy]>* % ',' 'tail' }
ok "tail" ~~ &t-zero, 'token: zero-iteration * % still lets the rest match';

# Grammar rules (ratchet + sigspace) with separated quantifiers: capture
# structure must be a per-iteration list on each named subrule.
grammar Csv {
    rule TOP { <item> * % ',' }
    token item { \w+ }
}
my $m = Csv.parse("aa, bb, cc");
ok $m, 'rule TOP { <item> * % \',\' } parses';
is $m<item>.elems, 3, 'three items captured';
is $m<item>[1].Str, 'bb', 'middle item text correct';

# The exponential-backtracking pathology pin: parsing an N-pair JSON-ish
# object under `rule pairlist { <pair> * % \, }` used to cost ~4x per
# added pair (6 pairs ~8s, 14 pairs would be ~9 hours). Linear now; if the
# exponential ever comes back this test hangs the file rather than failing
# an assertion, which CI's per-file timeout turns into a visible failure.
grammar JsonIsh {
    token TOP       { \s* <value> \s* }
    rule object     { '{' ~ '}' <pairlist> }
    rule pairlist   { <pair> * % \, }
    rule pair       { <string> ':' <value> }
    rule array      { '[' ~ ']' <arraylist> }
    rule arraylist  { <value> * % [ \, ] }
    proto token value {*};
    token value:sym<number> { '-'? [ 0 | <[1..9]> <[0..9]>* ] }
    token value:sym<true>   { <sym> }
    token value:sym<object> { <object> }
    token value:sym<array>  { <array> }
    token value:sym<string> { <string> }
    token string { ('"') ~ \" <str>* }
    token str { <-["\\\t\x[0A]]>+ }
}
my $doc = '{' ~ (1..14).map({ '"k' ~ $_ ~ '":' ~ $_ }).join(',') ~ '}';
my $j = JsonIsh.parse($doc);
ok $j, '14-pair object parses (linear, not exponential)';
is $j<value><object><pairlist><pair>.elems, 14, 'all 14 pairs captured';
is $j<value><object><pairlist><pair>[13]<value>.Str, '14', 'last pair value correct';
