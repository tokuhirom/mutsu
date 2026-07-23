use Test;

# `v` followed by digits is a version literal (`v6`, `v1.2.3`, `v1.2+`), but a
# `v<digits>` run that continues into a longer identifier via `-`/`'` is an
# ORDINARY identifier, not a version. The community IO::Socket::SSL binding
# defines `sub v4-split` / `sub v6-split`, so a too-greedy version lexer that
# swallowed `v4-` broke it (`No such method 'CALL-ME' for invocant Version`).

plan 9;

# Identifiers that merely start with v<digit>-word.
sub v4-split($u) { $u.split(':', 2).elems }
sub v6-split($u) { "v6:$u" }
is v4-split("a:b:c"), 2, 'v4-split is an identifier, not version v4 minus split';
is v6-split("x"), 'v6:x', 'v6-split is an identifier';

my $v4-var = 41;
is $v4-var + 1, 42, 'a $v4-... variable name is an identifier';

# Real version literals still parse (parenthesized so `.Str` is a method call,
# not a trailing version component — matching Raku).
is v6.^name, 'Version', 'v6 is a Version literal';
is (v1.2.3).Str, '1.2.3', 'v1.2.3 stringifies';
is (v6.c).Str, '6.c', 'v6.c alpha component';
is (v6c).Str, '6.c', 'v6c normalizes to 6.c';
is (v1.2+).Str, '1.2+', 'v1.2+ keeps the plus marker';
ok (v1.2.0 ~~ v1.2), 'version smartmatch still works';
