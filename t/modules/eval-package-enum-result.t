use Test;

# An enum declared in a package-like body pushes its Map, and nothing popped it:
# the Map parked at the frame's stack base and won over the real tail value of
# the unit, so `EVAL 'package P { enum E <A B> }; 42'` answered the Map (#12036).
# A statement of a body whose value nobody reads must leave nothing behind.
# Every expected answer is Rakudo 2026.09's.

plan 13;

is EVAL('package P4 { enum Status <S1 S2> }; 42'), 42, 'an enum in a package, then an expression';
is EVAL('module M1 { enum Status <S1 S2> }; "x"'), 'x', 'an enum in a module';
is EVAL('package A1 { package A2 { enum S <X Y> }; enum T <Z> }; 7'), 7, 'enums in nested packages';
is EVAL('module M2 { enum S <X Y>; enum T <Z W> }; "m"'), 'm', 'two enums in one module';
is EVAL('my package MP { enum Status <S1 S2> }; 9'), 9, 'a `my package`';
is EVAL('package P5 { enum Status <S1 S2> }; P5::Status::S2.value'), 1, 'the enum is still declared and usable';
is EVAL('INIT { enum IE <I1 I2> }; 11'), 11, 'an enum in an INIT body';

# Without a trailing statement the package itself is the value, as in rakudo.
is EVAL('package PE { enum E <A B>; our sub f { 1 } }').^name, 'PE', 'a package ending the unit is its own type object';

# Controls: the shapes that already answered correctly.
is EVAL('package P6 { 1 }; 42'), 42, 'a package without an enum';
is EVAL('enum E7 <A B>; 42'), 42, 'an enum outside a package';
is EVAL('my $x = 1; 42'), 42, 'plain statements';
is EVAL('class K1 { enum Status <S1 S2> }; "y"'), 'y', 'an enum in a class';

# The enum a final statement declares is still the unit's value.
is EVAL('enum E8 <A B>').keys.sort.join(','), 'A,B', 'a final enum is the value of the unit';
