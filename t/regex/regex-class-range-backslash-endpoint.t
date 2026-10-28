use Test;

# From PDF::Content: `.subst(/(<-[\0..\xFF]>)/, ...)`. A range endpoint in an
# enumerated class may be one of the single-character backslash escapes
# (\0 \t \e \f \r \b); the regex source tree used to refuse them, and the
# RakuAST round-trip then refused the whole module.
plan 9;

ok "\t" ~~ /<[\0..\x7f]>/, 'the class starts at \0';
nok "\x[100]" ~~ /<[\0..\xFF]>/, 'and stops at \xFF';
is "a\x[100]b\x[263A]".subst(/(<-[\0..\xFF]>)/, { '\u%04x'.sprintf($0.ord) }, :g),
    'a\\u0100b\\u263a', 'the negated class selects the wide characters';
is ("a\tb" ~~ /<[\t..\r]>/).Str, "\t", '\t..\r covers the control characters';
ok "\x[1b]" ~~ /<[\e..\x7f]>/, '\e as the lower endpoint';
nok "a" ~~ /<[\x1..\b]>/, '\b as the upper endpoint';
ok "\x[0c]" ~~ /<[\f..\r]>/, '\f..\r';
ok "x" ~~ /<[\0..\xFF]>/, "a plain character is inside \\0..\\xFF";
ok "\x[0]" ~~ /<[\0..\x[7]]>/, 'bracketed hex on the other side';

# vim: expandtab shiftwidth=4
