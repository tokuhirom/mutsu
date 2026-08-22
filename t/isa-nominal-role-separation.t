use Test;

# `.isa` follows only nominal MRO entries. Roles are matched by `.does` and
# smartmatch/type constraints instead.

nok 42.isa(Numeric), 'Int does not isa Numeric';
ok 42.does(Numeric), 'Int does Numeric';
nok 42.isa(Real), 'Int does not isa Real';
ok 42.does(Real), 'Int does Real';
nok (1 / 2).isa(Rational), 'Rat does not isa Rational';
ok (1 / 2).does(Rational), 'Rat does Rational';
nok 'x'.isa(Stringy), 'Str does not isa Stringy';
ok 'x'.does(Stringy), 'Str does Stringy';

my $array = [1, 2];
nok $array.isa(Positional), 'Array does not isa Positional';
ok $array.does(Positional), 'Array does Positional';
nok $array.isa(Iterable), 'Array does not isa Iterable';
ok $array.does(Iterable), 'Array does Iterable';
ok Array.does(Iterable), 'Array type object does Iterable';

my $hash = { a => 1 };
nok $hash.isa(Associative), 'Hash does not isa Associative';
ok $hash.does(Associative), 'Hash does Associative';
nok $hash.isa(Iterable), 'Hash does not isa Iterable';
ok $hash.does(Iterable), 'Hash does Iterable';
ok Hash.does(Iterable), 'Hash type object does Iterable';

my $f = sub { };
nok $f.isa(Callable), 'Sub does not isa Callable';
ok $f.does(Callable), 'Sub does Callable';

my $date = Date.new('2020-01-01');
nok $date.isa(Dateish), 'Date does not isa Dateish';
ok $date.does(Dateish), 'Date does Dateish';

enum Shade <red>;
nok red.isa(Enumeration), 'enum value does not isa Enumeration';
ok red.does(Enumeration), 'enum value does Enumeration';

# Concrete classes remain in `.isa` according to their actual MRO.
ok $array.isa(List), 'Array isa nominal List parent';
nok $array.isa(Seq), 'Array does not isa unrelated Seq';
ok $hash.isa(Map), 'Hash isa nominal Map parent';
ok $f.isa(Block), 'Sub isa nominal Block parent';
ok $f.isa(Routine), 'Sub isa nominal Routine parent';
ok $f.isa(Code), 'Sub isa nominal Code parent';

my $slip = (1, 2).Slip;
ok $slip.does(Iterable), 'Slip inherits List Iterable composition';

done-testing;
