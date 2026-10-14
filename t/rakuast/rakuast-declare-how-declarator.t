use v6;
use lib 't/lib';
use Test;

# A package declared with an EXPORTHOW::DECLARE declarator (`shouter Name { }`,
# registered by a `use`d module) is the ordinary RakuAST::Class whose
# `parsed-declarator` holds the keyword; the HOW is not part of the rendered
# tree (measured on rakudo 2026.09). EVAL of the tree builds the type with the
# module's HOW, as the parsed declaration does.

use DeclareShout;

plan 9;

my $ast = Q[use DeclareShout; shouter Loud { method hi() { "hi" } }; Loud.new.hi].AST;
my $class = $ast.statements[1].expression;

is $class.^name, 'RakuAST::Class', 'a DECLARE declarator builds a RakuAST::Class';
is $class.parsed-declarator, 'shouter', '.parsed-declarator is the keyword';
is $class.name.canonicalize, 'Loud', 'the name follows';
like $ast.statements[1].gist, /'parsed-declarator => "shouter",' \s+ 'name '/,
    'the gist renders parsed-declarator before the name';
is EVAL($ast), 'HI', 'EVAL of the tree applies the declarator HOW';

my $my = Q[use DeclareShout; my shouter Quiet { }].AST.statements[1].expression;
like $my.gist, /'scope' \s+ '=> "my",' \s+ 'parsed-declarator => "shouter",'/,
    'a scoped declaration renders scope, then parsed-declarator';

# The built-in declarators answer their own keyword and do not render it.
my $plain = Q[class Plain7 { }].AST.statements[0].expression;
is $plain.parsed-declarator, 'class', 'a plain class answers "class"';
unlike $plain.gist, /'parsed-declarator'/, 'and renders no parsed-declarator';

# Semantics of the parsed declaration in this unit.
shouter Greeter { method greet() { "hello" } }
is Greeter.new.greet, 'HELLO', 'the declared class has the module HOW';
