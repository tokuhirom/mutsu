use Test;
use experimental :rakuast;

plan 3;

# An infix a unit's own `use` imports is an overloaded operator for the statements
# after it, so `$v == $x` passes the variable's container (the `is rw` candidate
# wins over the `is readonly` one), as it does when the unit is parsed from source.
my $src = Q[use lib 't/lib';
use ExportedInfixFamilyBase;
my $dt = DateTime.new('2024-02-29T12:34:56Z');
my $n = Node.new(tag => 'x');
join ',', ($n == $dt), ($n == DateTime.new('2024-02-29T12:34:56Z'))];
is EVAL($src), 'DateTime(),DateTime() ro', 'EVAL of a string';
is EVAL($src.AST), 'DateTime(),DateTime() ro', 'EVAL of the tree';
is EVAL(Q[use lib 't/lib'; use ExportedInfixFamilyBase; 1 == 1].AST), True, 'a core operand still uses the core operator';
