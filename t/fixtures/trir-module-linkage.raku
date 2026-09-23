# Driver for t/vm/codegen/adr0110-trir-module-linkage.t: prints one line per
# shape, so the test can compare a TRIR-on run against a TRIR-off run and
# against hand-checked answers.
use lib $?FILE.IO.parent(2).add('lib').Str;
use TrirModuleLinkage;

for '5', '[1]', '[[1]]', '[[1],2]', '[ [1 , [2]], 3 ]', '[[[7]],[8,9]]' -> $t {
    my ($v, $pos) = TrirModuleLinkage::parse($t);
    say "$t => {$v.raku} pos=$pos";
}
say "count-ws => ", TrirModuleLinkage::count-ws("a  b   c d");
