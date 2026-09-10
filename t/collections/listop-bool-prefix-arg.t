use v6;
use Test;

# A paren-less call whose first argument starts with a boolean prefix (`!` or
# `?`) used to lose its whole argument list when the sub is declared LATER in
# the file. `is_user_sub` is false at that point, so the parse falls through to
# the "does the next token start a term?" gate, which listed `$`, `@`, digits,
# quotes, `(` ... but not a prefix operator. `later !1, 2` therefore parsed as
# two statements — a bare `later` term and an unrelated `(!1, 2)` list — and the
# call died with "Too few positionals passed ... got 0".
#
# rakudo's own Test.rakumod hits this in `unlike`:
#     my $ok := proclaim !($got ~~ $expected), $desc
# with `proclaim` defined 200 lines further down
# (todo/tickets/vendor-real-test-module.md).
#
# `!`/`?` are safe to add where a bare `+`/`-` is not: every Raku infix starting
# with them continues with an operator character (`!=`, `!~~`, `??`, `?|`, ...)
# and the negation metaops (`!eq`) continue with a letter, so requiring a sigil,
# paren, quote or digit right after excludes all of them.

plan 12;

my $one = 1;

sub call-bang-paren  { later-a !($one == 2), 'b' }
sub call-bang-lit    { later-a !1, 'b' }
sub call-bang-var    { later-a !$one, 'b' }
sub call-query-paren { later-a ?($one == 2), 'b' }
sub call-query-var   { later-a ?$one, 'b' }
sub call-query-lit   { later-a ?0, 'b' }

sub later-a($x, $y) { "$x.raku()/$y" }

is call-bang-paren,  'Bool::True/b',  'prefix ! on a paren group opens the argument list';
is call-bang-lit,    'Bool::False/b', 'prefix ! on a literal does too';
is call-bang-var,    'Bool::False/b', 'prefix ! on a variable does too';
is call-query-paren, 'Bool::False/b', 'prefix ? on a paren group does too';
is call-query-var,   'Bool::True/b',  'prefix ? on a variable does too';
is call-query-lit,   'Bool::False/b', 'prefix ? on a literal does too';

# A sub declared BEFORE the call always worked; keep it that way.
sub later-b($x, $y) { "$x.raku()/$y" }
is (later-b !$one, 'b'), 'Bool::False/b', 'a backwards reference is unchanged';

# Regression guards: the infixes that start with the same characters must not be
# swallowed as a prefixed argument.
is (5 != 3), True, 'infix != still parses';
is (5 !~~ Str), True, 'infix !~~ still parses';
is (1 ?| 0), True, 'infix ?| still parses';
is ($one == 1 ?? 'yes' !! 'no'), 'yes', 'the ?? !! ternary still parses';

# `elems - 1` style: a bare `-`/`+` stays an infix on a term, deliberately not
# covered by this gate.
is (pi - 1).Int, 2, 'a bare - after a term is still subtraction';
