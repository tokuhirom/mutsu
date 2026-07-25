use Test;
use lib 't/lib';
use NativeNameSigilless;

plan 10;

# A sigilless parameter named after a native type (`\str`, `\int`) read that
# TYPE OBJECT instead of its argument, but only for a routine compiled in a
# separate compilation unit: the BareWord compiler resolved an enclosing-scope
# sigilless binding by name EXCEPT when the name was a builtin type, and `str`
# and `int` are builtin type names. In raku a lexical sigilless binding shadows
# the type within its scope — which the same-frame branch already did.

is takes-str('Rakudo'),  'str=[Rakudo] name=Str', 'a \str parameter binds the argument, not the str type';
is takes-int(42),        'int=[42] name=Int',     'a \int parameter binds the argument';
is takes-num(1.5e0),     'num=[1.5] name=Num',    'a \num parameter binds the argument';
is takes-other('x'),     'zzz=[x] name=Str',      'a non-type-named parameter was already right';

# The String::Rotate shape end to end.
is rot('Rakudo', 3), 'udoRak', 'the coercion + defaulted-sigilless shape works';
is rot('Rakudo'),    'akudoR', 'and takes its default';
is rot('Rakudo', -1), 'oRakud', 'and a negative rotation';

# The loop-topic form the dist actually uses. (Collected by hand rather than with
# `do for`, which loses an imported sub's return value — a separate bug, see
# todo/tickets/do-for-loses-imported-sub-return-value.md.)
my $str = 'Rakudo';
my @got;
for ^3 { @got.push: rot($str, $_) }
is @got, ('Rakudo', 'akudoR', 'kudoRa'), 'called with the loop topic across a range';

# Nothing shadowing: the bare type name still names the type.
is type-still-visible(), 'str', 'an unshadowed native type name still resolves to the type';

# And in the main script, where the same-frame branch handles it.
sub local-str (Str \str) { str.^name }
is local-str('x'), 'Str', 'the same-frame case keeps working';
