use Test;

plan 6;

# DateTime attribute introspection and constructor tolerance needed by
# JSON round-tripping (JSON::Unmarshal 040 "DateTime as a definite").

ok DateTime.^attributes.elems >= 8, 'DateTime models its attributes';
ok DateTime.^attributes.first({ .has_accessor }),
    'DateTime attributes have accessors (ClassLike-style detection)';

my %names = DateTime.^attributes.map({ .name => True });
ok %names<$!year> && %names<$!hour> && %names<$!daycount>,
    'year/hour/daycount attributes present';

# Constructor accepts an undefined formatter (a JSON null round-trip) and a
# redundant daycount alongside full date fields.
my $d = DateTime.new(
    year => 2022, month => 7, day => 10,
    hour => 20, minute => 32, second => 12.345e0,
    timezone => -14400,
    daycount => 59770, formatter => Callable,
);
is $d.year, 2022, 'DateTime constructed with daycount+formatter extras';
is $d.hour, 20, 'hour set';
is $d.minute, 32, 'minute set';
