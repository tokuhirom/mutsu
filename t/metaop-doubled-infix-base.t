# A metaop's base operator is matched by longest spelling. mutsu's list was
# missing `^^`, `^` and `===`, so `Z^^` fell back to a *bare* `Z` over the term
# `^(^2)` — a different program, which died with X::Range::InvalidArg instead of
# zipping two booleans.
#
# Fixing that is what makes the term-position diagnosis below safe: while the
# scanner took only the `Z`, the `^^` arrived in what looked like term position
# and X::Syntax::DuplicatedPrefix rejected valid code
# (todo/tickets/duplicated-prefix-needs-metaop-aware-placement.md).
use Test;

plan 14;

is-deeply ((True, False) Z^^ (False, False)).List, (True, False), 'Z^^ zips the one-of infix';
is-deeply ((1, 2) Z^ (3, 4))».gist.List, ('one(1, 3)', 'one(2, 4)'), 'Z^ zips the junction infix';
is-deeply (1 Z=== 2).List, (False,), 'Z=== zips value identity';
is-deeply (1 X^^ 2).List, (Nil,), 'X^^ crosses the one-of infix';
is-deeply (1 R^^ 2), Nil, 'R^^ reverses the one-of infix';

# The longer spellings that share a prefix must still win.
is-deeply ((1, 2) Z+^ (3, 4)).List, (2, 6), 'Z+^ is still numeric xor, not Z+ then ^';
is-deeply ((1, 2) Z== (1, 3)).List, (True, False), 'Z== is still numeric equality, not Z===';
is-deeply ((1, 2) Z~~ (1, 3)).List, (True, False), 'Z~~ is unaffected';

# A doubled prefix character in *term* position is X::Syntax::DuplicatedPrefix.
throws-like 'my $x = ~~1', X::Syntax::DuplicatedPrefix, prefixes => '~~',
    'doubled ~ in term position';
throws-like 'say ^^5', X::Syntax::DuplicatedPrefix, prefixes => '^^',
    'doubled ^ in term position';
# rakudo reports only the first two characters, even for a longer run.
throws-like 'my $x = ^^^1', X::Syntax::DuplicatedPrefix, prefixes => '^^',
    'tripled ^ in term position';

# ...but the single prefixes, and the same characters used as infixes, are fine.
is-deeply (^3).List, (0, 1, 2), 'a single prefix ^ still means upto';
is ~42, '42', 'a single prefix ~ still stringifies';
ok (True ^^ False), 'infix ^^ is unaffected';
