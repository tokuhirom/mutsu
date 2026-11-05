use v6;
use lib 't/lib-return-spec-enum';
use Test;
use ReturnSpecEnum;

# SQL::Abstract: `role Term { method precedence(--> Precedence::Termlike) {} }`
# in the module that declares `enum Precedence`. A value in the `-->` position
# is the routine's return value, however the enum value is spelled.

plan 4;

is Leaf.new.precedence.raku, 'Precedence::Termlike', 'qualified enum value on a role method';
is qualified().raku, 'Precedence::Comma', 'qualified enum value on a sub';
is bare().raku, 'Precedence::Comma', 'bare enum value';
is fully().raku, 'Precedence::Rowlike', 'fully qualified enum value';

done-testing;
