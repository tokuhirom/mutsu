use v6;
use Test;

plan 8;

# The numbered-capture ($0/$1) counterpart of
# regex-optional-group-list-captures.t: when a `[...]?` group matches ZERO
# times, a POSITIONAL capture under a nested LIST quantifier (`*`/`+`/`%`)
# inside it still renders as an EMPTY LIST, while a positional capture under
# only the `?` (a plain, unquantified `(...)`) stays Nil. Found via
# Net::Netmask's IPv6 grammar (GH issue #8585): `[ (<h16>) +% ':']? '::'
# [ (<h16>) +% ':' ]?` failed to match '::1' at all because the unmatched
# leading group's $0 came back Nil instead of [], and `@$0 + @$1` on a Nil
# either produced a wrong count or (with plain arithmetic) errored out.
grammar G {
    token TOP { "/" [ (\w+) [ "/" (\w+) ]* ]? }
}

my $m = G.parse("/");
ok $m.defined, 'bare "/" parses';
nok $m[0].defined, 'plain positional directly under ? stays Nil at zero matches';
is-deeply $m[1], [], 'positional under a nested * renders as an empty list';

my $m2 = G.parse("/a");
is ~$m2[0], 'a', 'one-match ? group captures the direct positional';
is-deeply $m2[1], [], 'inner * with zero iterations is an empty list';

my $m3 = G.parse("/a/b");
is ~$m3[1][0], 'b', 'inner * with one iteration is a one-element list';

# The exact shape from #8585: a `+%`-quantified capture group wrapped in an
# optional `[...]?`. Both sides of a `::`-style separator must resolve their
# own numbered capture to an empty list (not Nil) when they matched nothing,
# so arithmetic like `@$0 + @$1` sees 0, not a Nil-context error.
grammar Sep {
    token TOP {
        [ (\d) +% ',']? '::' [ (\d) +% ',']? <?{ @$0 + @$1 <= 4 }>
    }
}
my $sm = Sep.parse('::1,2');
ok $sm.defined, 'leading +% group matching zero times still parses';
is-deeply ([+] @$sm[0]), 0, 'unmatched +% group sums to 0 via an empty list, not a Nil error';
