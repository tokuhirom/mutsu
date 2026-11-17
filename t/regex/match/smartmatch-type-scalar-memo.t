use Test;

# A built-in scalar smartmatched against a nominal type is answered from a
# memo keyed by the scalar's built-in type (#17876). These pin that the memo
# never answers a check whose result reads the value, or a name re-bound in
# scope: each case runs several times so a remembered answer would show.

plan 13;

sub all-same(&check, @values) { @values.map(&check).List }

is-deeply all-same({ $_ ~~ Positional }, <a b c>), (False, False, False), 'Str ~~ Positional';
is-deeply all-same({ $_ ~~ Stringy }, <a b c>), (True, True, True), 'Str ~~ Stringy';
is-deeply all-same({ $_ ~~ Str:D }, <a b>), (True, True), 'Str ~~ Str:D';
is-deeply all-same({ $_ ~~ Str:U }, <a b>), (False, False), 'Str ~~ Str:U';
is-deeply all-same({ $_ ~~ Numeric }, (1, 2.5, 1e0, True)), (True, True, True, True),
    'Int/Rat/Num/Bool ~~ Numeric';

# Types whose answer reads the value.
is-deeply all-same({ $_ ~~ UInt }, (5, -1, 7, -2)), (True, False, True, False), 'Int ~~ UInt';
# A native type accepts no boxed value (as in rakudo).
is-deeply all-same({ $_ ~~ uint8 }, (3, 300)), (False, False), 'Int ~~ uint8';
is-deeply all-same({ $_ ~~ int }, (1, 2**70)), (False, False), 'Int ~~ int';

subset Small of Int where * < 10;
is-deeply all-same({ $_ ~~ Small }, (5, 50, 6, 60)), (True, False, True, False), 'Int ~~ a subset';

enum Colour <red green>;
is-deeply all-same({ $_ ~~ Colour }, (0, 1, 0)), (False, False, False), 'Int ~~ an enum';

# A name re-bound in an inner scope means the inner binding there.
is-deeply all-same({ $_ ~~ Positional }, <x y>), (False, False), 'before the inner scope';
{
    my constant Positional = Str;
    is-deeply all-same({ $_ ~~ Positional }, <x y>), (True, True), 'a constant shadows the type';
}
{
    my class Stringy { }
    is-deeply all-same({ $_ ~~ Stringy }, <x y>), (False, False), 'a lexical class shadows the role';
}
