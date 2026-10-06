use v6;
use Test;

# `Version.parts` lists a `*` part as the Str "*", not as a `Whatever`: the
# `Whatever` is only how the version itself is compared (`.whatever`), and
# zef's DependencySpecification reads `.parts` and joins them back with `.`.

plan 18;

my $v = Version.new('1.2.*');

is $v.parts.raku, '(1, 2, "*")', 'a trailing * is the Str "*"';
is $v.parts[2].WHAT.raku, 'Str', '... a Str';
ok $v.parts[2] eq '*', '... equal to "*"';
nok $v.parts[2] ~~ Whatever, '... and not a Whatever';
is $v.parts[2].raku, '"*"', '... which .raku quotes';
is $v.parts[2].gist, '*', '... and .gist does not';
is $v.parts.map(*.^name).join(','), 'Int,Int,Str', 'Int, Int, Str';

is Version.new('*').parts.raku, '("*",)', 'a lone *';
is Version.new('1.*.3').parts.raku, '(1, "*", 3)', 'a * in the middle';
is Version.new('1.*+').parts.raku, '(1, "*")', 'a * before the + suffix';
is Version.new('2021.10.*').parts.raku, '(2021, 10, "*")', 'a longer version';
is Version.new('6.d').parts.raku, '(6, "d")', 'an alphabetic part is still a Str';
is Version.new('1.2.3').parts.raku, '(1, 2, 3)', 'a plain version is unchanged';

# The version itself still knows it has a whatever part.
ok $v.whatever, '.whatever is still True';
nok Version.new('1.2.3').whatever, '... and False without a * part';
is ~$v, '1.2.*', 'stringification is unchanged';

# zef's padding idiom rebuilds a version from `.parts`: a "*" joins as `*`.
is ~Version.new((|$v.parts, 0).join('.')), '1.2.*.0', 'the padding idiom keeps the *';
is-deeply Version.new((|$v.parts).join('.')).parts, $v.parts, 'parts round-trip through join';
