use Test;

# The methods Rakudo composes into every enum type (`Enumeration`,
# `NumericEnumeration`, `StringyEnumeration`) are rows of the method table,
# found by their owner (ADR-11276 §9.125).

plan 23;

enum E <x y z>;
enum S (p => 'q', r => 's');
enum N (cool => '42');

is y.key, 'y', 'key';
is y.value, 1, 'value';
is-deeply y.kv, ('y', 1), 'kv';
is y.pair.key ~ '=' ~ y.pair.value, 'y=1', 'pair';
is y.gist, 'y', 'gist';
is y.raku, 'E::y', 'raku';
is y.Str, 'y', 'Str of an Int-valued member is its key';
is p.Str, 'q', 'Str of a Str-valued member is its value';
is p.gist, 'p', 'gist of a Str-valued member is its key';
is y.pred, x, 'pred';
is y.succ, z, 'succ';
is x.pred, x, 'pred of the first member is itself';
is z.succ, z, 'succ of the last member is itself';
is r.pred, p, 'pred of a Str-valued member';
ok y.ACCEPTS(1), 'ACCEPTS compares by value';
nok y.ACCEPTS(2), 'ACCEPTS of another value';
is y.Int, 1, 'Int';
is y.Numeric, 1, 'Numeric';
is y.Real, 1, 'Real';
is cool.Int, 42, 'Int of a Str-valued member coerces the value';

role Greet { method hello { 'hi ' ~ self.key } }
enum G does Greet <g h>;
is h.hello, 'hi h', "a composed role's method sees the rows";

my Array enum A (:a[1, 2]);
is a.elems, 2, "an Array-valued member answers its base type's methods";
is a.key, 'a', 'and the rows';
