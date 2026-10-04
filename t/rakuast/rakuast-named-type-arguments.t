use Test;

# Rakudo 2026.09 represents a named argument in a parameterized type as
# RakuAST::ColonPair::Value and preserves it through EVAL.
plan 6;

my $class-ast = Q|role NamedArgSource[:$y] { method y { $y } }; class NamedArgClass does NamedArgSource[:y(2)] { }; NamedArgClass.new.y|.AST;
my $class-type = $class-ast.statements[1].expression.traits[0].type;
isa-ok $class-type, RakuAST::Type::Parameterized, 'class does has a parameterized type';
isa-ok $class-type.args.args[0], RakuAST::ColonPair::Value, 'its named argument is a colonpair';
is EVAL($class-ast), 2, 'class does keeps the named role argument';

my $role-ast = Q|role NamedArgParent[:$y] { method y { $y } }; role NamedArgChild does NamedArgParent[:y(3)] { }; class NamedArgUser does NamedArgChild { }; NamedArgUser.new.y|.AST;
my $role-type = $role-ast.statements[1].expression.traits[0].type;
isa-ok $role-type, RakuAST::Type::Parameterized, 'role does has a parameterized type';
isa-ok $role-type.args.args[0], RakuAST::ColonPair::Value, 'its named argument is a colonpair';
is EVAL($role-ast), 3, 'role does keeps the named role argument';
