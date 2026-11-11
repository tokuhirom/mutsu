use Test;
use lib 't/lib';
use LoopParamShadowsFileLexical;

# A pointy `for` parameter in a module routine is the innermost binding of its
# name: it shadows the module's same-named file-scope (or package-block) `my`
# while the loop runs, and the outer variable is back once it ends. mutsu read
# the file-scope lexical inside the loop (#17429).

plan 7;

is-deeply direct(), [1, 2, 'file'], 'a loop parameter shadows a file-scope lexical';
is-deeply nested(), [2, 1, 'file'], 'nested loops with the same parameter name';
is-deeply outer-closure(), ['file'], 'a closure from outside the loop still sees the file-scope lexical';
is-deeply with-my(), [10, 20, 'fileN'], 'arithmetic on the parameter';
is-deeply topic(), ['1file', '2file'], 'a topic loop leaves the file-scope lexical visible';
is-deeply nil-param(), ['1', 'Any'], 'a parameter bound to Nil';
is-deeply in-package(), [1, 'pkg'], 'a loop parameter shadows a package-block lexical';
