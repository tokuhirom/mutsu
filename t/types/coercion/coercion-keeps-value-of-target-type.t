use Test;

# A coercion type `T(S)` / `T()` calls `.T` only on a value that is not
# already a `T`. A `Bool` already is an `Int` (and `Numeric`, `Cool`), and an
# allomorph (`IntStr`, `NumStr`, `RatStr`) already is both of its types, so
# they bind unchanged; a value of another type is still converted (#18856).
plan 17;

sub ib(Int(Bool) $x) { $x.^name }
sub ia(Int() $x)     { $x.^name ~ ':' ~ $x }
sub sa(Str() $x)     { $x.^name ~ ':' ~ $x }
sub si(Str(Int) $x)  { $x.^name }
sub na(Numeric() $x) { $x.^name }
sub ba(Bool() $x)    { $x.^name ~ ':' ~ $x }
sub nu(Num() $x)     { $x.^name }

is ib(True), 'Bool', 'Int(Bool) keeps a Bool';
is ia(False), 'Bool:False', 'Int() keeps a Bool';
is na(True), 'Bool', 'Numeric() keeps a Bool';
is si(<1>), 'IntStr', 'Str(Int) keeps an IntStr';
is sa(<1>), 'IntStr:1', 'Str() keeps an IntStr';
is ia(<3>), 'IntStr:3', 'Int() keeps an IntStr';
is nu(<1e0>), 'NumStr', 'Num() keeps a NumStr';

is sa(True), 'Str:True', 'Str() still converts a Bool';
is sa(42), 'Str:42', 'Str() still converts an Int';
is ia("42"), 'Int:42', 'Int() still converts a Str';
is ia(2.5), 'Int:2', 'Int() still converts a Rat';
is nu(True), 'Num', 'Num() still converts a Bool';
is ba(0), 'Bool:False', 'Bool() still converts an Int';
is ba(<0>), 'Bool:False', 'Bool() converts an allomorph';
is ba(True), 'Bool:True', 'Bool() keeps a Bool';

class C { method m(Int(Bool) $x) { $x.^name } }
is C.new.m(True), 'Bool', 'a method parameter keeps a Bool';

my Int() $v = True;
is $v.^name, 'Bool', 'a coercion-typed variable keeps a Bool';
