use Test;

plan 4;

# Perl 5 trailing substitution flags are obsolete; use pre-delimiter adverbs.
throws-like 's/a/b/i', X::Obsolete,
    'trailing /i flag on substitution is obsolete';
throws-like 's/a/b/g', X::Obsolete,
    'trailing /g flag on substitution is obsolete';

# The valid Raku adverb form still works.
{
    my $s = "AbA";
    $s ~~ s:i/a/X/;
    is $s, "XbA", 's:i/.../.../ applies the ignorecase adverb';
}
{
    my $s = "aaa";
    $s ~~ s/a/b/;
    is $s, "baa", 'a plain substitution still works';
}
