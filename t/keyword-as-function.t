use Test;

# A statement-control keyword immediately followed by `(` (no whitespace) is a
# `keyword()`-as-function mistake. Raku rejects it with an X::Comp::Group whose
# first sorrow is X::Syntax::KeywordAsFunction.

plan 13;

for <if unless while until for given when with without loop> -> $kw {
    throws-like
        $kw ~ '() {}',
        X::Comp::Group,
        "$kw\() is rejected",
        sorrows => sub (@s) { @s[0] ~~ X::Syntax::KeywordAsFunction };
}

# The offending word is exposed on the sorrow.
throws-like
    q[if() {}],
    X::Comp::Group,
    'sorrow exposes .word',
    sorrows => sub (@s) { @s[0].word eq 'if' };

# Legitimate uses with whitespace still parse and run.
lives-ok { my $r = (if-marker => 1); }, 'identifier containing keyword still ok';

is-deeply
    (do { my @out; for 1, 2 -> $x { @out.push: $x }; @out }),
    [1, 2],
    'for with whitespace still works';
