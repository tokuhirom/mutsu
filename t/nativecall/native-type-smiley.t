use v6;
use Test;

# A type smiley on a *lowercase* native type name in TERM position
# (`array:D`, `int:U`, `num64:_`). The parser only accepted a smiley after a
# name starting with an ASCII uppercase letter, so every one of these died with
# `X::Undeclared::Symbols`, even though the very same spelling already parsed in
# *signature* position (`sub f(array:D \x)`). See
# todo/deep/nativehelpers-blob-moarvm-guts.md Gap B.
#
# The gate is the native-type name set, not "is lowercase": an arbitrary
# lowercase identifier followed by `:D...` is an ordinary colonpair adverb, so
# `foo:Debug` must keep parsing as one.

plan 22;

is (array:D).^name, 'array:D', 'array:D parses and names itself in term position';
is (array:U).^name, 'array:U', 'array:U too';
is (int:D).^name,   'int:D',   'int:D';
is (int:U).^name,   'int:U',   'int:U';
is (num:D).^name,   'num:D',   'num:D';
is (num64:D).^name, 'num64:D', 'num64:D (a digit-suffixed native name)';
is (str:D).^name,   'str:D',   'str:D';
is (byte:D).^name,  'byte:D',  'byte:D';
is (uint32:U).^name, 'uint32:U', 'uint32:U';
is (atomicint:D).^name, 'atomicint:D', 'atomicint:D';

is (array:D).WHAT.gist, '(array:D)', '.WHAT keeps the smiley';
is (int:D).gist, '(int:D)', '.gist matches raku';
is (array:D).DEFINITE, False, 'a smiley type object is itself a type object';

# Smartmatching a real native array against the smiley type. This is the
# contract the todo file recorded as unparseable.
my array[uint8] $a .= new(1, 2);
ok $a ~~ array:D, 'a defined native array matches array:D';
# Rakudo answers True for the *literal* `$a ~~ array:U` spelling, but False for
# both `(array:U).ACCEPTS($a)` and `my $t = array:U; $a ~~ $t` -- i.e. its
# literal form constant-folds the definiteness check away for native types.
# mutsu answers the self-consistent False everywhere.
nok $a ~~ array:U, 'and not array:U';

# The uppercase path must be unchanged.
is (Int:D).^name, 'Int:D', 'Int:D still works';
ok 5 ~~ Int:D, 'Int:D still accepts a defined Int';
nok 5 ~~ Int:U, 'Int:U still rejects it';

# A lowercase identifier that is NOT a native type name keeps its adverb
# parse -- the smiley arm must not swallow `:D`-prefixed colonpairs.
sub adverbed($x, *%adv) { %adv.keys.sort.join(',') }
is adverbed(1, :Debug), 'Debug', 'a :Debug adverb is still an adverb';

# ... and even on a native type name, a *longer* adverb starting with `:D`
# is an adverb, not a smiley.
sub str($x, *%adv) { %adv.keys.sort.join(',') }
is str(1, :Deep), 'Deep', ':Deep after a native type name is an adverb, not str:D';

# Signature position (which always worked) still does.
sub takes-int(int:D $x) { 'd' }
is takes-int(5), 'd', 'int:D in signature position still binds';
sub takes-arr(array:D \x) { x.^name }
is takes-arr($a), 'array[uint8]', 'array:D in signature position still binds';

# vim: expandtab shiftwidth=4
