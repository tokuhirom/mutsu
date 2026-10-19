use Test;
use lib 't/lib';
use CanParentMcp;

# A custom ClassHOW can add a parent while composing a declarator-created
# class. The added parent's methods must be visible to ClassHOW.can as well as
# ordinary dispatch and MRO introspection (MCP::Server, #13919).

plan 6;

mcp CanChild { }

is CanChild.new.hello, 'hi', 'the custom-HOW class dispatches to its added parent';
is-deeply CanChild.^mro.map(*.^name),
    <CanChild CanParent Any Mu>, 'the added parent is present in the MRO';
is CanChild.^can('hello').elems, 1, '.^can finds the inherited method';
is CanChild.^can('new').elems, 1, '.^can finds the constructor';
is CanChild.HOW.can(CanChild, 'hello').elems, 1,
    'HOW.can and .^can agree about the inherited method';
is CanParent.^can('hello').elems, 1, 'the parent still reports its local method';
