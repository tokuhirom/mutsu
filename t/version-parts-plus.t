use v6;
use Test;

plan 10;

# Version introspection methods used by zef's DependencySpecification
# version matching (spec-matcher pads .parts and checks .plus).

my $v123 = v1.2.3;
is-deeply $v123.parts, (1, 2, 3), 'v-literal .parts';
is-deeply Version.new("6.d").parts, (6, "d"), 'string part';
is-deeply Version.new("1.2+").parts, (1, 2), '.parts excludes the + suffix';
is Version.new("2021.10.*").parts.raku, '(2021, 10, *)', 'Whatever part';

ok Version.new("1.2+").plus, '.plus True with + suffix';
nok Version.new("1.2").plus, '.plus False without suffix';
nok $v123.parts === Nil, 'parts is defined';

ok Version.new("1.2.*").whatever, '.whatever True with a * part';
nok Version.new("1.2").whatever, '.whatever False without * part';

# The zef spec-matcher padding idiom round-trips.
my $v = Version.new("1.2");
my $padded = Version.new((|$v.parts, |(0 xx 1), ("+" if $v.plus)).join('.'));
is ~$padded, '1.2.0', 'padding idiom builds 1.2.0';

done-testing;
