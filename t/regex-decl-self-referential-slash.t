use Test;

# An embedded `:my $c = $/;` declaration inside a slash-delimited regex is
# Main-slang code, just like a `{ ... }` code block: a `/` inside it (here the
# `/` of `$/`, the in-progress match object) must not terminate the regex.
# Regression: `/ (a) b {} :my $c = $/; /` used to fail with "Regex not
# terminated" because the closing-delimiter scanner had no awareness of the
# `:my ... ;` declarator clause and saw the `/` of `$/` as the regex's own
# closing delimiter (only `{ ... }` code blocks were protected before).
#
# The first fix for that made a second, distinct construct regress:
# `:my token NAME { … }` (and `:our`/`:constant`/`:let`/`:temp` + `rule`/
# `regex`) is a *block-form* declarator -- it declares a lexically-scoped
# named sub-rule and is terminated by BODY's own closing `}`, NOT by a `;`
# (`roast/S05-modifier/my.t`'s `:my token SIGN { <[+-]> }` has no trailing
# `;` at all). A first version of the fix assumed every `:my ...` clause was
# `;`-terminated and scanned past the whole rest of the file looking for one,
# breaking that construct with "Regex not terminated" too. The scanner now
# distinguishes the two shapes (see `block_form_decl_prefix_len` in
# src/parser/primary/regex/scan.rs).

plan 16;

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

# Block-form `:my token NAME { ... }` (regression: an earlier version of this
# fix assumed every `:my` clause ends in `;`, which broke this construct --
# see roast/S05-modifier/my.t). This form is only legal inside a
# {}-bracket-delimited regex literal (raku rejects it inside a slash-
# delimited one with "Strange text after block"), so it is exercised via
# `rx { ... }` / `my token NAME { ... }` here, not `/ ... /`.
lives-ok {
    "+123.456e10" ~~ rx {
        :my token SIGN { <[+-]> }
        :my token MANTISSA { \d+ '.'? \d* | '.' \d+ }
        :my token EXPONENT { <[eE]> <SIGN>? \d+ }
        <SIGN>? <MANTISSA> <EXPONENT>?
    };
}, 'multiple consecutive block-form :my token declarators (bracket-delimited rx) parse';

ok "+123.456e10" ~~ rx {
    :my token SIGN { <[+-]> }
    :my token MANTISSA { \d+ '.'? \d* | '.' \d+ }
    :my token EXPONENT { <[eE]> <SIGN>? \d+ }
    <SIGN>? <MANTISSA> <EXPONENT>?
}, ':my terminates upon }';

# The scalar form must keep working when it appears directly inside a
# bracket-delimited token body too (not just slash-delimited regexes above).
lives-ok {
    my token hasmy {
        :my $y = ' yack';
        b $y $y
    }
}, 'scalar-form :my (bracket-delimited token) still parses alongside the block form';

{
    my token hasmy2 {
        :my $y = ' yack';
        b $y $y
    }
    ok 'b yack yack' ~~ &hasmy2, 'scalar-form :my (bracket-delimited token) still matches correctly';
}

done-testing;
