use Test;

# A `--> Type:D` / `--> Type:U` return type is checked on the light call
# paths (found in MCP::Server::Tool::Web's tokenizer, whose small helpers are
# `my sub is-alpha(Str $ch --> Bool:D)`). The verdicts must be the general
# return check's, called many times so the cached light paths run.

plan 14;

my sub rd(Int $x --> Int:D) { $x }
my sub ru($x --> Int:U) { $x }
my sub rb(Str $ch --> Bool:D) { $ch eq 'a' }
my sub rnil(--> Int:D) { Nil }
my sub rfail(--> Int:D) { fail 'nope' }
my sub rany($x --> Any:D) { $x }
class Sub is Int { }

my $sum = 0;
$sum += rd($_) for ^200;
is $sum, 19900, 'Int:D return of an Int, many calls';

my $trues = 0;
$trues++ if rb($_ %% 2 ?? 'a' !! 'b') for ^200;
is $trues, 100, 'Bool:D return of a comparison, many calls';

for ^50 { ru(Int) }
is ru(Int).raku, 'Int', 'Int:U return of the type object';
is ru(Sub).raku, 'Sub', 'Int:U return of a subclass type object';
throws-like { ru(5) }, X::TypeCheck::Return, 'Int:U rejects an instance';
throws-like { for ^50 { rd($_) }; rd(Int) }, X::TypeCheck::Return,
    'Int:D rejects the type object after warm calls';
is (try rd(Int)) // $!.message,
    'Type check failed for return value; expected Int:D but got Int (Int)',
    'the message names the smiley type';

is rnil().raku, 'Nil', 'Nil passes a :D return type';
for ^20 { rnil() }
is rnil().raku, 'Nil', 'Nil passes a :D return type (warm)';
ok rfail() ~~ Failure, 'a Failure passes a :D return type';

is rany(42), 42, 'Any:D return of an Int';
is rany('x'), 'x', 'Any:D return of a Str';
throws-like { rany(Any) }, X::TypeCheck::Return, 'Any:D rejects Any';
ok rany(Sub.new(3)) == 3, 'Any:D return of an instance';
