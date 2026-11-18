use Test;

# A small statically typed sub with a built-in nominal return type
# (`--> Bool:D`, `--> Str`) runs on TRIR, which checks the returned value the
# same way the light call path does. Each sub is called in a loop so it is
# served from its compiled chunk, then once more for the answer checked.

plan 18;

my sub flag(Str:D $ch --> Bool:D) { $ch eq ' ' ?? True !! False }
my sub same(Str $ch --> Str) { $ch }
my sub wrong(Str $ch --> Bool:D) { $ch }
my sub int-ret($x --> Int) { $x }
my sub undef-ok($x --> Int:U) { $x }
my sub def-only($x --> Int:D) { $x }
my sub ret-early($x --> Str) { return 'early' if $x; 'late' }
my sub ret-bad($x --> Str) { return 42 if $x; 'late' }
my sub ret-nil($x --> Int:D) { Nil }
my sub ret-fail($x --> Int:D) { fail 'nope' }

for ^300 { flag(' '); same('a'); int-ret(1); undef-ok(Int); def-only(2); ret-early(0) }

is flag(' '), True, '--> Bool:D passes a Bool';
is flag('x'), False, '--> Bool:D passes False';
is same('abc'), 'abc', '--> Str passes a Str';
ok same(Str) === Str, '--> Str passes the Str type object';
throws-like { wrong('q') }, X::TypeCheck::Return, '--> Bool:D rejects a Str';
throws-like { wrong('q') }, X::TypeCheck::Return,
    message => /'expected Bool:D but got Str'/, 'with the expected and the got type';
is int-ret(5), 5, '--> Int passes an Int';
throws-like { int-ret('s') }, X::TypeCheck::Return, '--> Int rejects a Str';
ok undef-ok(Int) === Int, '--> Int:U passes a type object';
throws-like { undef-ok(3) }, X::TypeCheck::Return, '--> Int:U rejects a defined Int';
is def-only(7), 7, '--> Int:D passes a defined Int';
throws-like { def-only(Int) }, X::TypeCheck::Return, '--> Int:D rejects a type object';
is ret-early(1), 'early', 'an early return is checked and passes';
is ret-early(0), 'late', 'the fall-off value is checked and passes';
throws-like { ret-bad(1) }, X::TypeCheck::Return, 'an early return of the wrong type dies';
ok ret-nil(1) =:= Nil, 'Nil passes any return type';
my $f = ret-fail(1);
ok $f ~~ Failure, 'a Failure passes any return type';
$f.so;
my $total = 0;
$total++ if flag($_ %% 2 ?? ' ' !! 'x') for ^1000;
is $total, 500, 'a hot typed-return sub keeps answering';
