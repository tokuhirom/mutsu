use Test;

# `[TypeName]` is an array literal containing a type object, NOT a reduction
# metaop. The reduction parser used to accept any bareword as the operator, so
# `[Any].raku` parsed as a reduction with op "Any" applied to `$_.raku` and
# returned Nil. Uppercase-initial barewords (type/class names) must parse as
# array literals; lowercase named infixes (`[min]`, `[max]`) and user-declared
# infixes (even uppercase) still reduce.

plan 14;

# --- type objects in brackets are array literals ---------------------------

is [Any].raku,        '[Any]',          '[Any] is an array literal';
is [Int].raku,        '[Int]',          '[Int] is an array literal';
is [Exception].raku,  '[Exception]',    '[Exception] is an array literal';
is [Any].elems,       1,                '[Any] has one element';
is [Int][0].^name,    'Int',            '[Int][0] is the Int type object';
is-deeply [Str, Int, Num].map(*.^name).List, ('Str', 'Int', 'Num'),
    'multi-type array literal';

# --- standard reductions still work ----------------------------------------

is ([+] 1, 2, 3),     6,   '[+] reduction';
is ([*] 1..5),        120, '[*] reduction';
is ([max] 4, 2, 8),   8,   '[max] reduction';
is ([min] 4, 2, 8),   2,   '[min] reduction';
is ([gcd] 12, 8),     4,   '[gcd] reduction';
is ([~] 'a', 'b'),    'ab', '[~] reduction';

# --- user-declared infix (uppercase) still reduces -------------------------

{
    sub infix:<Plus>($a, $b) { $a + $b }
    is ([Plus] 1, 2, 3), 6, 'user-declared uppercase infix reduces';
}

# --- lowercase named reduction not in the core list still parses -----------

is ([lcm] 4, 6), 12, '[lcm] reduction';
