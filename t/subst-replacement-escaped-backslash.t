use Test;

# A replacement `\\` (escaped backslash → one literal backslash) followed by a
# capture var must keep the backslash: `S/(x)/\\$0/` yields `\x`, not `x`. The
# replacement was double-unescaped — `normalize_subst_replacement` collapsed
# `\\`→`\`, then the interpolation pass read the resulting `\$` as an escaped `$`
# and dropped the backslash entirely. (P5quotemeta `S:g/(<[...]>)/\\$0/`.)

plan 12;

# S/// (non-destructive) with $0 after \\
is (S/ (a) /\\$0/ given "a"), "\\a", 'S/// : \\ before $0 keeps the backslash';
is (S:g/ (<[ab]>) /\\$0/ given "ab"), "\\a\\b", 'S:g/// : \\ before $0, each match';

# s/// (destructive)
my $d = "a"; $d ~~ s/ (a) /\\$0/;
is $d, "\\a", 's/// : \\ before $0 keeps the backslash';

# .subst with a closure replacement (per-match $0).
is "ab".subst(/(<[ab]>)/, {"\\$0"}, :g), "\\a\\b", '.subst closure replacement \\$0';

# \\ before a hex codepoint char class (the P5quotemeta shape)
is (S:g/ ( <[ \x[3164] ]> ) /\\$0/ given "\x[3164]"), "\\\x[3164]",
    'S:g/// : \\ before $0 with a hex codepoint class';

# A lone literal backslash in the replacement (no capture after) still works.
is (S/ a /\\/ given "a"), "\\", 'lone \\ replacement is one backslash';
is (S/ a /\\X/ given "a"), "\\X", '\\ before a literal char';
is (S/ (a) /$0\\/ given "a"), "a\\", '\\ after $0';

# Two escaped backslashes → two literal backslashes.
is (S/ (a) /\\\\$0/ given "a"), "\\\\a", 'double \\\\ before $0 = two backslashes';

# \\ before n is a literal backslash + n, NOT a newline.
is (S/ a /\\n/ given "a"), "\\n", '\\ before n is literal backslash + n';

# A bare \n escape (single backslash) still becomes a newline.
is (S/ a /\n/ given "a"), "\n", 'bare \n is a newline';

# $0 interpolation without a backslash is unaffected.
is (S/ (a) /X$0X/ given "a"), "XaX", 'plain $0 interpolation still works';
