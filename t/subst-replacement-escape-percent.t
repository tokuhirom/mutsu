use v6;
use Test;

# In an `s///` replacement, `%` is a sigil (a bare `%(...)` would interpolate
# a hash), so `\%` is an escaped literal percent — the backslash is stripped,
# like `\$` and `\@`. Regression: mutsu handled `\$`/`\@` but left `\%`
# literal ("\%"). (Language/regexes.rakudoc)

plan 4;

{
    $_ = 'foo';
    s/foo/\%(/;
    is $_, '%(', '\% in a replacement becomes a literal %';
}
{
    $_ = 'x';
    s/x/\$/;
    is $_, '$', '\$ still works (baseline)';
}
{
    $_ = 'y';
    s/y/\@/;
    is $_, '@', '\@ still works (baseline)';
}
{
    $_ = 'z';
    s/z/100\%/;
    is $_, '100%', '\% works mid-string too';
}
