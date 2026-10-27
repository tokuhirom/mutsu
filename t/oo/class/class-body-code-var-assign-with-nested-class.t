use Test;
use lib $*PROGRAM.parent(3).add('lib');
use CodeVarSplit;

# From Timezones::ZoneInfo (Timezones::ZoneInfo::State): `&fmt = sub { ... }` in
# a class body whose method declares a nested class died with "Cannot modify an
# immutable value", and the method then could not call `fmt`.
plan 2;
is CodeVarSplit.new.go("a"), "a!", 'a body-level my &code is assigned and callable from a method';
class Local {
    my &k;
    method m { class Deep { }; k(2) }
    &k = sub ($n) { $n * 3 };
}
is Local.new.m, 6, 'same for a braced class in the program';
