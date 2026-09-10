use Test;

# The replacement half of a substitution is a qq-like string, not a regex,
# so text like `<[`, `]>`, `<`, `#`, and `'` in the replacement is literal —
# it must NOT be interpreted as a regex character class / assertion / comment.
# Regression: `s:g/ '[' /<[/` used to fail parsing because the replacement `<[`
# was scanned as the start of a `<[...]>` character class, consuming past the
# closing `/`. (Test::Selector builds its glob->regex this way.)

plan 9;

{
    my $s = 'a';
    $s ~~ s:g/ (.) /<[/;
    is $s, '<[', 'replacement `<[` is literal (global)';
}

{
    my $s = 'a';
    $s ~~ s:g/ (.) /]>/;
    is $s, ']>', 'replacement `]>` is literal (global)';
}

{
    my $s = 'a';
    $s ~~ s/ (.) /<[/;
    is $s, '<[', 'replacement `<[` is literal (single)';
}

{
    my $s = 'a';
    $s ~~ s/ (.) /</;
    is $s, '<', 'replacement `<` is literal';
}

{
    my $s = 'a';
    $s ~~ s/ (.) /a#b/;
    is $s, 'a#b', 'replacement with `#` is literal, not a comment';
}

{
    my $s = 'x';
    $s ~~ s/x/'a'/;
    is $s, "'a'", 'single quotes in replacement are literal';
}

# The `{...}` closure form still protects its contents (including a nested
# string that contains the delimiter), so this must keep working.
{
    my $s = 'x';
    $s ~~ s/x/{ "a/b" }/;
    is $s, 'a/b', 'closure replacement with delimiter inside a string';
}

# The exact glob->regex build sequence used by Test::Selector.
{
    my $want = 'a*b?c[d]';
    $want ~~ s:g/ '?' /./;
    $want ~~ s:g/ '*' /.*?/;
    $want ~~ s:g/ '[' /<[/;
    $want ~~ s:g/ ']' /]>/;
    is $want, 'a.*?b.c<[d]>', 'Test::Selector glob->regex build';
}

# Non-destructive S/// replacement is likewise a string.
{
    my $s = 'a';
    my $r = S/ (.) /<[/ given $s;
    is $r, '<[', 'S/// replacement `<[` is literal';
}
