use Test;
use lib 't/lib';
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 2;

is EVAL(q[need ImportEnvScope; { import ImportEnvScope; greet() }].AST),
    'hello', 'a routine imported in an EVAL block resolves there';
dies-ok { EVAL(q[need ImportEnvScope; { import ImportEnvScope; greet() }; greet()].AST) },
    'the imported routine does not resolve outside that block';
