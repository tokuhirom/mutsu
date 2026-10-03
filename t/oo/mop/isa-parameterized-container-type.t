use Test;

# `.isa` with a parameterized container type compares the typed container's
# own type. Found in Data::Reshapers: `my Hash @a = ...; @a.isa(Array[Hash])`.

plan 7;

my Hash @aoh = {a => 1}, {a => 2};
ok @aoh.isa(Array[Hash]), 'my Hash @a isa Array[Hash]';
ok @aoh.isa(Array), 'and still isa Array';
nok @aoh.isa(Array[Int]), 'but not Array[Int]';
my @plain;
nok @plain.isa(Array[Hash]), 'an untyped array is not Array[Hash]';
my Hash %hoh = x => %(a => 1);
ok %hoh.isa(Hash[Hash]), 'my Hash %h isa Hash[Hash]';
my Int %h;
nok %h.isa(Hash[Str]), 'Hash[Int] is not Hash[Str]';
my %oh{Int};
ok %oh.isa(Hash[Any,Int]), 'an object hash isa Hash[Any,Int]';
