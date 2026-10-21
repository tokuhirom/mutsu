use Test;
use lib $?FILE.IO.parent(2).add('lib').Str;

# A module's own top-level `use` of a module with a `sub EXPORT` hook imports
# the hook's values into that module only. The program that `use`s the
# importing module does not see them (#14140). Checked against rakudo.

use HookImportingModule;

plan 4;

is module-reads-hook(), 'hf hv-val a,b', "the importing module's routine reads the hook's values";
is (try EVAL 'hf()') // 'not visible', 'not visible', "the hook's routine is not the program's";
is (try ::('&hf')()) // 'not visible', 'not visible', '...nor through an indirect lookup';
is (try EVAL '$hv') // 'not visible', 'not visible', "nor is the hook's scalar";
