use Test;

plan 6;

# The type object of an Int-based enum is an Int type, one step below Int (#13243).
enum E <a b>;
enum S <x y>;

ok E ~~ Int, 'an Int enum type object smartmatches Int';
sub i(Int $x) { "ok" }
is i(E), 'ok', 'an Int parameter accepts the enum type object';

multi mt(Int:U) { "int" }
multi mt(E)     { "fmt" }
is mt(E), 'fmt', 'the enum candidate out-ranks Int:U for the enum type object';
is mt(Int), 'int', 'Int still picks the Int:U candidate';

enum Str-E (p => 'p', q => 'q');
nok Str-E ~~ Int, 'a Str-valued enum type object is not an Int';
ok S ~~ UInt, 'an Int enum type object is still a UInt';
