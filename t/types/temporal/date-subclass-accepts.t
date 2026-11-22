use Test;

# `Date.ACCEPTS` / `DateTime.ACCEPTS` for an instance of a subclass compare
# by value as the parent does, not by identity (#18818).

plan 14;

class T is Date { }
class U is DateTime { }
class S is Date { multi method ACCEPTS(Str $s) { True } }

ok  T.new(2020,1,1) ~~ T.new(2020,1,1), 'a Date subclass matches the same day';
nok T.new(2020,1,1) ~~ T.new(2020,1,2), 'a Date subclass refuses another day';
nok T.new(2020,1,1) ~~ Date.new(2020,1,1), 'a Date is a value type: the type is part of it';
nok Date.new(2020,1,1) ~~ T.new(2020,1,1), 'the other way round too';
ok  T.new(2020,1,2).ACCEPTS(T.new(2020,1,2)), '.ACCEPTS called directly';
ok  S.new(2020,1,1) ~~ S.new(2020,1,1), 'a user multi ACCEPTS adds to the Date one';
ok  "x" ~~ S.new(2020,1,1), 'the user candidate still answers its topic';
ok  U.new(2020,1,1,0,0,0) ~~ U.new(2020,1,1,0,0,0), 'a DateTime subclass matches the same instant';
ok  U.new(2020,1,1,0,0,0) ~~ DateTime.new(2020,1,1,0,0,0), 'DateTime.ACCEPTS compares instants across subclasses';
ok  DateTime.new(2020,1,1,1,0,0,:timezone(3600)) ~~ DateTime.new(2020,1,1,0,0,0),
    'DateTime.ACCEPTS is the same instant in another time zone';
ok  DateTime.new(2020,1,1,0,0,0) ~~ Date.new(2020,1,1), 'a Date accepts a DateTime of that day';
nok Date.new(2020,1,1) ~~ DateTime.new(2020,1,1,0,0,0), 'a DateTime refuses a Date';

my $t = T.new(2020,1,1);
my $hit = do given T.new(2020,1,1) { when $t { 'hit' }; default { 'miss' } };
is $hit, 'hit', 'when with a Date subclass matcher';
is (T.new(2020,1,1), T.new(2020,1,2)).grep($t).elems, 1, 'grep with a Date subclass matcher';
