use Test;
use MONKEY-SEE-NO-EVAL;
use lib 't/lib';

plan 6;

# A module's `my class` / `my role` stays visible to the module's own closures
# when the module is loaded by a `use` inside an EVAL (#17129): the load binds the
# type in the EVAL's import region, which the closures' static link skips.
class T1 { }
is EVAL('use EvalLexicalTypes; install(T1); T1.viaadd'), 'hid',
    'an added method sees the module lexical class (called inside the EVAL)';
is T1.viaadd, 'hid', 'and after the EVAL';
nok T1.marked(42), 'a module lexical role is visible too';

is EVAL('use EvalLexicalTypes; class T2 { }; install(T2); T2.viaadd'), 'hid',
    'a class declared in the EVAL as well';

is EVAL('use EvalLexicalTypes; make-hid()'), 'hid', 'a plain module routine';
is (try EVAL 'use EvalLexicalTypes; Hid.new.hi') // 'undeclared', 'undeclared',
    'the module lexical class is still not visible to the EVAL itself';
