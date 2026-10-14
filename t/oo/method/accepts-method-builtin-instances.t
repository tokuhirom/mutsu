use Test;

# #13333: `ACCEPTS` of an instance of a built-in class reaches Rakudo's
# candidate: an `Exception` (and its subclasses) is an `Any`, so it accepts
# only itself; a subclass of `Date` answers `Date.ACCEPTS`.

plan 8;

my $e = Exception.new;
ok $e.ACCEPTS($e), 'an Exception accepts itself';
nok $e.ACCEPTS(Exception.new), 'an Exception does not accept another';

class X2 is Exception {}
my $x = X2.new;
ok $x.ACCEPTS($x), 'an Exception subclass accepts itself';
nok $x.ACCEPTS(X2.new), 'an Exception subclass does not accept another';
ok $x.ACCEPTS(1) === False, 'and answers a Bool';

class DD is Date {}
my $d = DD.new(2020, 1, 1);
ok $d.ACCEPTS($d), 'a Date subclass accepts itself';
nok $d.ACCEPTS(Date.new(2020, 1, 1)), 'a Date subclass does not accept a plain Date';
ok $d ~~ $d, 'smartmatch agrees';
