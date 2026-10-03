use Test;
# From Monitor::Monit: `ServiceType($v)` with a Str "3" from XML.
plan 9;

enum ST <A B C>;
enum SS (a => 'x', b => 'y');
enum N (p => 1.5, q => 2.5);

is ST("1").raku, 'ST::B', 'Str of an Int value finds the variant';
is ST(1).raku, 'ST::B', 'Int still works';
ok ST("B") ~~ Failure, 'a variant name is not a lookup key';
ok ST("9") ~~ Failure, 'unknown Str value fails';
is SS("x").raku, 'SS::a', 'Str-valued enum by Str value';
ok SS("1") ~~ Failure, 'Int-like Str on a Str enum fails';
is N("1.5").raku, 'N::p', 'Num-valued enum by Str value';
my Str $s = "2";
is ST($s).raku, 'ST::C', 'Str variable';
ok ST("1e0") ~~ Failure, 'non-canonical Str does not match';
