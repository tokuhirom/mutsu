use Test;

# `Numeric.ACCEPTS` compares numerically, so an object topic with its own
# `.Numeric` matches a number or an enum value with the same numeric value.
# mutsu compared types and said False. Reduced from Lumberjack, whose Message
# class has `method Numeric { $!level }` and is checked with
# `$message ~~ Lumberjack.default-level`.

plan 6;

enum Level <Off Fatal Error>;
class M { has $.n = 2; method Numeric { $!n } }
class E { has $.level = Error; method Numeric { $!level } }

ok M.new ~~ 2, 'object ~~ Int';
nok M.new ~~ 3, 'object ~~ other Int';
ok M.new ~~ Error, 'object ~~ enum value';
ok E.new ~~ Error, 'Numeric returning an enum value';
class LJ { has Level $.default-level is rw = Error }
ok E.new ~~ LJ.new.default-level, 'enum value through an rw accessor';
ok 2.ACCEPTS(M.new), 'Int.ACCEPTS directly';
