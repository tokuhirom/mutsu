use Test;

plan 4;

class EveryHour is DateTime { }

my $value = EveryHour.new(2020, 1, 1, 0, 0, 0);
my $later = $value + Duration.new(60);
my $earlier = $value - Duration.new(60);

isa-ok $later, EveryHour, 'DateTime addition preserves a subclass';
isa-ok $earlier, EveryHour, 'DateTime subtraction preserves a subclass';
is $later.^name, 'EveryHour', 'the addition result keeps its concrete type name';
is $earlier.^name, 'EveryHour', 'the subtraction result keeps its concrete type name';
