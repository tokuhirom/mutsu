use Test;
use lib 't/lib';
use CanParentMcp;

# A custom ClassHOW can add a parent while composing a declarator-created
# class whose name shadows a builtin. The synthetic Any root must stay last.
# (MCP::Server, #13920.)

plan 3;

mcp Complex { method process { 'p' } }

is-deeply Complex.^mro.map(*.^name),
    <Complex CanParent Any Mu>, 'the custom parent precedes the synthetic Any root';
is Complex.new.hello, 'hi', 'the builtin-name class inherits the custom parent method';
is Complex.new.process, 'p', 'the builtin-name class constructs its declared class instance';
