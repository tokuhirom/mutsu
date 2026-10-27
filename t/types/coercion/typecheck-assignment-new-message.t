use Test;

plan 5;

# `X::TypeCheck::Assignment.new(:symbol, :got, :expected)` built by hand (Red's
# `Red::Model.new` rejects a mistyped column that way) has rakudo's message,
# and `throws-like ... message => rx/value/` can see it.
my $e = X::TypeCheck::Assignment.new(symbol => '$!value', got => 42, expected => Str);
is $e.message, 'Type check failed in assignment to $!value; expected Str but got Int (42)',
    'message of a hand-built exception';

my $caught;
try { die X::TypeCheck::Assignment.new(symbol => '$!value', got => "x", expected => Int); CATCH { default { $caught = $_ } } }
isa-ok $caught, X::TypeCheck::Assignment, 'thrown instance keeps its type';
is $caught.message, 'Type check failed in assignment to $!value; expected Int but got Str ("x")',
    'message of a thrown hand-built exception';

throws-like { die X::TypeCheck::Assignment.new(symbol => '$!value', got => 42, expected => Str) },
    X::TypeCheck::Assignment, message => rx/value/;

class Bla { has Str $.value is rw }
my $b = Bla.new(value => "x");
try { $b.value = 42; CATCH { default { $caught = $_ } } }
like $caught.message, /'$!value'/, 'interpreter-raised message is unchanged';
