use Test;

plan 4;

class DateHash is Hash does Associative[Cool, DateTime] { }
my %date-hash := DateHash.new;
is %date-hash.of.^name, 'Cool', 'Associative role value type drives Hash subclass .of';

my $date-hash = DateHash.new;
is $date-hash.of.^name, 'Cool', 'ordinary instance dispatch reads Associative role value type';

class CoolArray is Array does Positional[Cool] { }
my @cool-array := CoolArray.new;
is @cool-array.of.^name, 'Cool', 'Positional role value type drives Array subclass .of';

class InheritedDateHash is DateHash { }
my %inherited := InheritedDateHash.new;
is %inherited.of.^name, 'Cool', 'subclass inherits composed Associative value type for .of';
