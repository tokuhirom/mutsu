use Test;

# `$obj but R` on an instance is a copy of `$obj` that does `R` (rakudo's
# `.clone.does`): a parametric role's parameter defaults are bound for its
# `TWEAK` and its methods, as for `$obj does R` (#17765). An untyped role
# parameter left unsupplied reads `Mu`, and a mixin renamed with `.^set_name`
# renders its instances by the new name. Every expectation was checked
# against rakudo.

plan 14;

my @tweaks;
role R[Bool:D :$reset = True, :$x] {
    method show { "reset=$reset x={$x.raku}" }
    submethod TWEAK { @tweaks.push("tweak reset=$reset") }
}
class C { has $.n }

my $orig = C.new(n => 1);
my $e = $orig but R;
is $e.show, 'reset=True x=Mu', '`but` binds the default and leaves the other parameter Mu';
is-deeply @tweaks, ['tweak reset=True'], 'and its TWEAK sees the default';
is $e.^name, 'C+{R}', 'the copy is of the mixin type';
is $e.n, 1, 'and keeps the attributes';
is $orig.^name, 'C', 'the original is untouched';
nok $orig === $e, 'the result is a copy';

my $f = C.new but R[:!reset, :x(5)];
is $f.show, 'reset=False x=5', 'explicit arguments bind as given';

my $o = C.new;
$o does R;
is $o.show, 'reset=True x=Mu', '`does` reads the same';

role P[$y?] { method y { $y.raku } }
role T[Int :$z] { method z { $z.raku } }
class A does P { }
class B does T { }
is A.y, 'Mu', 'an unsupplied optional positional role parameter is Mu';
is B.z, 'Int', 'a typed one is its type object';

role Tag { }
my $tagged = C.new(n => 2) but Tag;
$tagged.^set_name('Tagged');
is $tagged.^name, 'Tagged', '.^set_name on a `but` copy renames its type';
is $tagged.raku, 'Tagged.new(n => 2)', 'and its .raku renders the new name';

class Plain { has $.v }
Plain.^set_name('Renamed');
is Plain.new(v => 3).raku, 'Renamed.new(v => 3)', 'so does a renamed class';
is Plain.^name, 'Renamed', 'whose .^name reports it';
