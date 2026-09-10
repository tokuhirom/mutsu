use Test;

plan 8;

class Foo is Int { }

ok Foo.new() == 0, "Int subclass .new without args defaults to numeric zero";
ok Foo.new(42) == 42, "Int subclass .new preserves numeric payload";

my int $native;
ok $native === 0, "uninitialized native int is strictly identical to zero";

is lsb(0b01000), 3, "lsb() function dispatches to numeric lsb behavior";
is msb(0b01011), 3, "msb() function dispatches to numeric msb behavior";

ok Int ~~ UInt, "undefined Int type object smartmatches UInt";
ok UInt ~~ Int, "UInt type object smartmatches Int";

throws-like { Int.new(Int) }, Exception, "Int.new(Int) throws";
