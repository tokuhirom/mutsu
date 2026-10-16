use Test;

# `Collation`'s methods are rows of `Collation`, found by their owner; `set`
# writes the instance's shared cell, so every alias sees it (ADR-11276 §9.131).

plan 8;

my $c = $*COLLATION;
is $c.collation-level, 85, 'collation-level';
is ($c.primary, $c.secondary, $c.tertiary, $c.quaternary).join, '1111', 'the four levels';
$c.set(:!tertiary);
is $c.collation-level, 69, 'set changes the level';
is $*COLLATION.tertiary, 0, 'set through a variable is seen through $*COLLATION';
is $c.gist,
    'collation-level => 69, Country => International, Language => None, primary => 1, secondary => 1, tertiary => 0, quaternary => 1',
    'gist';
my @cs = $*COLLATION;
@cs[0].set(:tertiary);
is $*COLLATION.tertiary, 1, 'set through an array element';
is <b A a B>.collate.join(' '), 'a A b B', 'collate reads the setting';
ok $c.can('collation-level'), '.can finds a row';
