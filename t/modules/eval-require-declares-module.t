use v6;
use Test;
use lib 't/lib';

# `require Module` declares the name `Module` for the code after it, in an
# EVAL'd unit as in a program: the undeclared-name check does not refuse it.

plan 5;

throws-like { EVAL('RequireGreeter.new') }, X::Undeclared::Symbols,
    'without the require the name is undeclared';

is EVAL('require RequireGreeter; RequireGreeter.new(name => "W").greet'), 'Hello, W!',
    'EVAL string: require then use the type';
is EVAL('{ require RequireGreeter; RequireGreeter.new(name => "B").greet }'), 'Hello, B!',
    'EVAL string: inside a block';
is EVAL(Q{require RequireGreeter; RequireGreeter.new(name => "A").greet}.AST), 'Hello, A!',
    'EVAL of the AST';
throws-like { EVAL('require RequireGreeter; NoSuchRequired.new') }, X::Undeclared::Symbols,
    'another name is still undeclared';
