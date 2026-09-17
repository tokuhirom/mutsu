use Test;

# `[ '/' <bar> ]*` (a quantified group around a named subrule, matching zero
# times) used to die with "Null regex not allowed" instead of matching an
# empty string. Root cause: fixing a *different* bug (a bare regex literal at
# a routine's tail position not implicitly matching `$_`, see
# t/regex/bare-regex-routine-tail-implicit-match.t) initially over-widened the
# same desugar to a plain `Expr::Literal` wrapping a `Regex` value -- the
# shape a `token`/`rule`/`regex` declaration's own body carries internally.
# That corrupted a named-subrule's LTM (longest-token-matching) lookup: it
# called the subrule expecting back its own compiled pattern, got a match
# result instead, and re-derived an empty pattern string from it, which the
# regex parser then rejected as a null regex. Pinned narrowly here (a
# self-contained grammar, no vendored dependency) so the fix cannot regress
# without a local test noticing.

plan 4;

grammar TG {
    token TOP  { <foo> }
    token foo  { [ '/' <bar> ]* }
    token bar  { <[a..z]>* }
}

my $m1 = TG.subparse("xyz", :rule<foo>);
ok $m1.defined, 'a quantified group around a named subrule matches zero times';
is $m1.Str, '', 'the zero-width match is the empty string';

my $m2 = TG.subparse("/abc/def", :rule<foo>);
ok $m2.defined, 'the same rule also matches one or more repetitions';
is $m2.Str, '/abc/def', 'consuming every repetition';
