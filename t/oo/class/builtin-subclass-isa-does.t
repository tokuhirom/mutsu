use Test;

plan 10;

# #12907: type tests on an instance of a class deriving from a built-in
# scalar type ask about the subclass, not about its payload.
role R { method rm { } }
class S does R is Str { method sm { } }
my $s = S.new(value => "x");
ok $s.isa(S), '.isa(the subclass)';
ok $s.isa('S'), '.isa(the subclass name)';
ok $s.isa(Str), '.isa(the built-in base)';
nok $s.isa(Int), 'not .isa(an unrelated type)';
ok $s.does(R), '.does(a composed role)';
ok $s.can('sm'), '.can(a subclass method)';
ok $s.can('uc'), '.can(a base method)';

class I is Int { }
my $i = I.new(5);
ok $i.isa(I), 'Int subclass .isa(itself)';
ok $i.isa(Int), 'Int subclass .isa(Int)';
isa-ok $s, 'S';
