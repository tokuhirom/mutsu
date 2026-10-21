use Test;
use lib 't/lib';

# A `my role` declared in a loaded module's class body is lexical to that
# body: it leaves no type-only key in the loading program's scope (the keys
# were copied with every program frame, #14331), yet the class's methods
# still use it.
plan 3;

use Issue14331ClassBodyRole;

is Issue14331ClassBodyRole::Outer.new.make.^name,
    'Int+{Issue14331ClassBodyRole::Outer::Inner14331[Int]}',
    "the class's method still mixes in its my role";
is MY::.keys.grep(*.contains('Inner14331')).elems, 0,
    "the program's scope holds no key for the body's my role";
is (try EVAL 'Inner14331') // 'not visible', 'not visible',
    'the role is not visible to the program';
