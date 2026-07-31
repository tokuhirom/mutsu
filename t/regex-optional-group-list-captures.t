use v6;
use Test;

plan 6;

# When a `[...]?` group matches ZERO times, a name under a nested LIST
# quantifier (`*`/`+`) inside it still renders as an EMPTY LIST, while a name
# under only the `?` stays absent (Nil). Cro::Uri's path-absolute action
# iterates `@$<segment>` — with Nil that loop runs once and appended a bogus
# '/' (parse-ref('/') returned path '//').
grammar G {
    token TOP { "/" [ <seg> [ "/" <seg2> ]* ]? }
    token seg { \w+ }
    token seg2 { \w+ }
}

my $m = G.parse("/");
ok $m.defined, 'bare "/" parses';
nok $m<seg>.defined, 'name directly under ? stays undefined at zero matches';
is-deeply $m<seg2>, [], 'name under a nested * renders as an empty list';

my $m2 = G.parse("/a");
is ~$m2<seg>, 'a', 'one-match ? group captures the direct subrule';
is-deeply $m2<seg2>, [], 'inner * with zero iterations is an empty list';

my $m3 = G.parse("/a/b");
is ~$m3<seg2>[0], 'b', 'inner * with one iteration is a one-element list';
