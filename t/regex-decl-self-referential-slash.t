use Test;

# An embedded `:my $c = $/;` declaration inside a slash-delimited regex is
# Main-slang code, just like a `{ ... }` code block: a `/` inside it (here the
# `/` of `$/`, the in-progress match object) must not terminate the regex.
# Regression: `/ (a) b {} :my $c = $/; /` used to fail with "Regex not
# terminated" because the closing-delimiter scanner had no awareness of the
# `:my ... ;` declarator clause and saw the `/` of `$/` as the regex's own
# closing delimiter (only `{ ... }` code blocks were protected before).

plan 12;

lives-ok {
    "aba" ~~ / (a) b {} :my $c = $/; /;
}, 'a :my declarator whose RHS is $/ parses';

{
    "aba" ~~ / (a) b {} :my $c = $/; /;
    is ~$/, 'ab', 'the whole regex still matched to the end';
    is ~$/[0], 'a', 'the capture group is intact';
}

# :our and :constant share the same delimiter-scanning path.
lives-ok {
    "aba" ~~ / (a) b {} :our $x = $/; /;
}, 'a :our declarator whose RHS is $/ parses';

# $/ followed by a postfix (already worked before this fix -- regression check).
lives-ok {
    "aba" ~~ / (a) b {} :my $c = $/.Str; /;
}, 'a :my declarator whose RHS is $/.Str (postfix) still parses';

# A plain :my declarator (no $/ reference) must keep working.
lives-ok {
    "aba" ~~ / (a) b :my $c = 1; /;
}, 'a plain :my declarator without $/ still parses';

# A :my declarator without a wrapping {} block must keep working too.
lives-ok {
    "aba" ~~ / (a) b :my $c = $/; /;
}, 'a :my declarator with no preceding {} block still parses';

# $/ inside an actual { ... } code block is unaffected (pre-existing coverage,
# re-asserted here alongside the new :my case).
lives-ok {
    'ab' ~~ / a { my $z = $/; } b /;
}, 'a $/ reference inside a regex code block still parses';

# The pre-existing $-anchor / real-closing-delimiter disambiguation must not
# regress: a trailing `$` immediately before the true closing `/` is still the
# end-of-string anchor, not a misfired match on the new :my-decl skip.
ok 'foo' ~~ /foo$/, 'a trailing $ anchor immediately before the real close still works';
nok 'foobar' ~~ /foo$/, 'the $ anchor still rejects a non-matching trailing string';

# Substitution patterns share the same underlying scanner (scan_to_delim_inner).
{
    my $s = 'aba';
    lives-ok {
        $s ~~ s/ (a) b {} :my $c = $/; /X/;
    }, 'a :my declarator whose RHS is $/ parses inside a substitution pattern';
    is $s, 'Xa', 'the substitution itself still applied correctly';
}

done-testing;
