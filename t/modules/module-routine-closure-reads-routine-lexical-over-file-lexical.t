use Test;
use lib $?FILE.IO.parent(2).add('lib').Str;

# A closure created in a module routine that names one of the routine's own
# lexicals (a `my`, a pointy loop parameter) means that binding, not the
# module's same-named file-scope `my` or package `our` (#17466). The reads
# used to answer the file-scope value, and the writes went to the file-scope
# variable. Every expectation was checked against rakudo.

use RoutineClosureShadowsFileLexical;

plan 19;

is loop-closures(), '1,2', 'a loop parameter read by closures run after the loop';
is loop-sub(), '1', 'a loop parameter read by an anonymous sub called in the loop';
is loop-rw(), '10,20', 'a `<->` loop parameter written through';
is block-read(), 'inner', "a block reads the routine's `my`";
is pointy-read(), 'inner', "a pointy block reads the routine's `my`";
is sub-read(), 'inner', "an anonymous sub reads the routine's `my`";
is escaped-read()(), 'inner', 'a block returned from the routine still reads its `my`';
is nested-read(), 'inner', 'a closure nested in a closure reads the routine `my`';
is block-local-read(), '1,2', "a closure reads its loop body's `my`";
is block-write(), 5, "a block's assignment reaches the routine's `my`";
is block-increment(), 3, "a block's increments reach the routine's `my`";
is hash-write(), 'a', "a block's key store reaches the routine's `my %x`";
is array-write(), '3', "a block's push reaches the routine's `my @x`";
is our-shadow-read(), 'mine', "the routine's `my \$o` shadows the package's `our \$o`";
is our-shadow-write(), 9, "a write to the routine's `my \$o` stays there";
is file-read(), 'file', 'the file-scope variable is untouched by the writes above';
is our-read(), 'our-file', 'the package variable is untouched by the writes above';
is sibling-block-read(), 'file', "a sibling block's ended `my` does not shadow the file scope";
is loop-closures(), '1,2', 'a second call binds the loop parameter afresh';
