use Test;

# `Parameter.type` is the nominal type; the definedness smiley is reported by
# `.modifier`, and `.raku` spells both.

plan 10;

class Mem { }
sub f(Mem:D $x, Mem:U $y, Int:_ $z, Int $w) { }
my @p = &f.signature.params;

ok @p[0].type === Mem, ':D parameter type is the nominal type';
is @p[0].modifier, ':D', '... and the smiley is its modifier';
ok @p[1].type === Mem, ':U parameter type is the nominal type';
is @p[1].modifier, ':U', '... with modifier :U';
ok @p[2].type === Int, ':_ parameter type is the nominal type';
is @p[3].modifier, '', 'a plain parameter has no modifier';
is @p[0].raku, 'Mem:D $x', '.raku keeps the smiley';

class K { method m(K:D: Int $x) { } }
my $inv = K.^find_method('m').signature.params[0];
ok $inv.type === K, 'a :D invocant type is the nominal type';
is $inv.modifier, ':D', '... with modifier :D';
is $inv.type.REPR, 'P6opaque', 'the type answers its own REPR';
