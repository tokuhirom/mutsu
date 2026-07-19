use v6;
use Test;

# The reduction / `&[op]` metaop path numified a numeric string with `f64::parse`,
# which rejects surrounding whitespace and silently fell back to 0.0 — so
# `&[==].("1 ", 1)` was False even though the infix `"1 " == 1` is True (Raku
# numeric-string coercion trims whitespace). This checks the two paths agree.

plan 12;

# &[==] as a callable agrees with infix ==.
is (&[==].("1 ", 1)),  ("1 " == 1),  '&[==] trailing-space string vs Int';
is (&[==].(" 1", 1)),  (" 1" == 1),  '&[==] leading-space string vs Int';
is (&[==].("1", 1)),   ("1" == 1),   '&[==] no-space string vs Int';
is (&[==].(1, "1 ")),  (1 == "1 "),  '&[==] Int vs trailing-space string';
ok  (&[==].("  42  ", 42)),          '&[==] surrounding-space string';
nok (&[==].("2 ", 1)),               '&[==] distinct values stay distinct';

# The reduce form.
ok  ([==] "1 ", 1, 1.0),  '[==] reduces a whitespace string with numeric peers';
ok  (&[<].("1 ", 2)),     '&[<] numifies a whitespace string';

# The unique/repeated doc examples that surfaced it (Type/Any.rakudoc).
is ("1", 1, "1 ", 2).unique(with => &[==]).elems, 2,
    '.unique(with => &[==]) dedups whitespace-numeric strings';
is-deeply ("1", 1, "1 ", 2).unique(as => Int, with => &[==]).List, ("1", 2),
    '.unique(as => Int, with => &[==]) matches raku';
is-deeply (1, -1, 2, -2, 3).repeated(:as(&abs), :with(&[==])).List, (-1, -2),
    '.repeated(:as, :with) over plain Ints';

# Plain all-Int reductions are unaffected.
ok ([==] 3, 3, 3), '[==] over equal Ints still True';
