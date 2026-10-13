use Test;

plan 12;

# UInt is `subset UInt of Int where { not .DEFINITE or $_ >= 0 }`: a type object
# of any Int type passes, and so does a non-negative Int-subclass instance.
sub f(UInt $x) { "ok" }
class MyInt is Int {}
enum E <a b>;

is f(Bool), "ok", "UInt accepts the Bool type object";
is f(IntStr), "ok", "UInt accepts the IntStr type object";
is f(MyInt), "ok", "UInt accepts an Int-subclass type object";
is f(MyInt.new(5)), "ok", "UInt accepts a non-negative Int-subclass instance";
is f(a), "ok", "UInt accepts an enum value";
is f(E), "ok", "UInt accepts an Int-based enum type object";
dies-ok { f(<-3>) }, "UInt rejects a negative IntStr";
dies-ok { f(MyInt.new(-1)) }, "UInt rejects a negative Int-subclass instance";
dies-ok { f(Any) }, "UInt rejects Any";

sub i(Int $x) { "ok" }
is i(E), "ok", "Int accepts an Int-based enum type object";
ok E ~~ Int, "an Int-based enum type object smartmatches Int";
dies-ok { i(Any) }, "Int rejects Any";
