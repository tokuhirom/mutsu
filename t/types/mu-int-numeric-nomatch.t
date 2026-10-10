use Test;

plan 7;

class A { has $.x = 1 }
my $a = A.new;

try $a.Int;
isa-ok $!, X::Multi::NoMatch, '.Int on a plain instance is X::Multi::NoMatch';
like $!.message, /'Cannot resolve caller Int(A:D: )'/, '.Int message names the class';

try $a.Numeric;
isa-ok $!, X::Multi::NoMatch, '.Numeric on a plain instance is X::Multi::NoMatch';
like $!.message, /'Cannot resolve caller Numeric(A:D: )'/, '.Numeric message names the class';

{
    my $w = '';
    CONTROL { when CX::Warn { $w = .message; .resume } }
    is A.Int, 0, 'type object .Int still warns and gives 0';
    like $w, /'uninitialized value of type A'/, 'with the uninitialized warning';
}

class B { method Int { 42 } }
is B.new.Int, 42, 'a user-defined .Int still wins';

