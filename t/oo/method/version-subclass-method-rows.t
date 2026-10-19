use Test;

# `Version.ACCEPTS` is a row that takes a `Version` argument or an instance of
# a `Version` subclass, and an `is Version` subclass answers `Version`'s rows
# on the version it carries (ADR-11276 slice 5).

plan 11;

ok Version.new("1.2").ACCEPTS(Version.new("1.2.3")), 'a version matches a longer one';
ok Version.new("1.2+").ACCEPTS(Version.new("1.3")), 'a + version matches a later one';

class V is Version {}

ok V.new("1.2").ACCEPTS(Version.new("1.2")), 'a subclass instance as the matcher';
nok V.new("1.2") ~~ Version.new("1.2.1"), 'a subclass instance as the topic';
nok Version.new("1.2") ~~ V.new("1.2.5"), 'a subclass instance as the argument';
is-deeply V.new("1.2").parts, (1, 2), 'parts of a subclass instance';
is V.new("1.2.3").raku, 'v1.2.3', 'raku';
is V.new("1.2").Str, '1.2', 'Str';
is V.new("1.2").^name, 'V', 'the instance keeps its class';
is V.new("1.2") cmp V.new("1.3"), Less, 'cmp of two subclass instances';

is 10.base("2"), '1010', 'Int.base coerces a Str base';
