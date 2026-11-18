use Test;

# A RakuAST node type constrains a `my` variable as it constrains a
# parameter (CSS::Specification::Compiler: `my RakuAST::Name $name := ...`).

plan 5;

my RakuAST::Name $n := RakuAST::Name.from-identifier('x');
isa-ok $n, RakuAST::Name, 'a bound RakuAST::Name variable';

my RakuAST::Regex $r = RakuAST::Regex::Literal.new('a');
isa-ok $r, RakuAST::Regex::Literal, 'a supertype constraint takes a subtype node';

my RakuAST::Name $u;
ok $u === RakuAST::Name, 'an unassigned one holds the type object';

throws-like { my RakuAST::Name $bad = 42 }, X::TypeCheck::Assignment,
    'the constraint is checked';

throws-like 'my RakuAST::NoSuchNode $x;', X::Comp, 'an unknown RakuAST name is still undeclared';
