use Test;
use lib $*PROGRAM.parent(3).add('lib');
use ExportOverride::Wrap;

plan 4;

# A `sub EXPORT` override is an import of the units that ran the `use`, not of
# the module that exported it: the module's own routines (and the blocks in
# them, even when the script pulls a lazy `.map` result) keep calling what
# they imported under the name.
is greet(1), 'base:1', 'a non-list argument reaches the added dispatchee';
is greet([1, 2]), 'wrap[base:1,base:2]', 'the wrapper calls the imported routine';
is greet([1, [2, 3]]), 'wrap[base:1,base:2 3]', 'no recursion into the wrapper';
is direct(5), 'base:5', 'a plain routine of the exporting module';
