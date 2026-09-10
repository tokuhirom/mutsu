use Test;

# `[&foo]` (single bracket) is an array literal containing the sub `&foo`,
# NOT a reduction. The reduction parser used to flatten both `[&foo]` and
# `[[&foo]]` to op "&foo" and treat the single-bracket form as a reduction
# (which returned Nil), so passing `[&mw]` as a positional argument bound to
# an `@`-sigil parameter failed with a type-check error. In Raku:
#   * `[&foo]`        -> array literal (a term)
#   * `2 [&foo] 3`    -> `&foo` used as an infix operator
#   * `[[&foo]] @a`   -> reduction with `&foo` as the operator

plan 12;

sub dbl($x) { $x * 2 }
sub add($a, $b) { $a + $b }
sub cat($a, $b) { $a ~ $b }

# --- single-bracket callable is an array literal ---------------------------

is [&dbl].elems,        1,    '[&dbl] is a one-element array';
is [&dbl][0].(21),      42,   '[&dbl][0] is the sub itself';
is [&dbl, &add].elems,  2,    'multi-element callable array literal';

# passing `[&mw]` as a positional arg to an @-sigil parameter (the bug)
sub takes-array(Str:D $s, @mw = List.new) { "$s:{ @mw.elems }" }
is takes-array('a', [&dbl]),        'a:1', 'single-callable array binds to @param';
is takes-array('a', [&dbl, &add]),  'a:2', 'two-callable array binds to @param';
is takes-array('a'),                'a:0', 'default empty array still works';

# combined with a &callback parameter (the Humming-Bird get/post shape)
sub route(Str:D $path, &cb, @mw = List.new) { "{ &cb($path) }/{ @mw.elems }" }
is route('/', -> $p { $p.uc }, [&dbl]), '//1', 'route(&cb, [&mw]) binds correctly';

# --- reduction and infix forms still work ----------------------------------

is ([[&add]] 1, 2, 3, 4),  10,    '[[&add]] reduces with a sub operator';
is ([[&cat]] <a b c>),     'abc', '[[&cat]] reduces strings';
is (2 [&add] 3),           5,     '[&add] as an infix operator';
is (2 [&add] 3 [&add] 4),  9,     'chained [&add] infix';

# symbolic reductions unaffected
is ([+] 1, 2, 3),          6,     '[+] symbolic reduction still works';
