use lib 't/lib';
use Test;

# A routine the program declares under a core routine's name is lexical to the
# program (#17433): a module's own by-name call keeps reaching the core routine
# (`Test.rakumod`'s `_push_vars` calls `item`; EEL 0.3's `t/44` declares a
# `sub item`), while the program, and the callbacks it hands to the module,
# still reach the program's own.

plan 9;

use CoreRoutineCaller;

sub item($v) { 42 }

is module-item().raku, '$[1, 2]', "the module's item is the core one";
is item(1), 42, "the program's item is its own";
is run-it({ item(1) }), 42, 'a block callback the module runs';
is run-it(&item.assuming(1)), 42, 'a code value';
class C { method go { item(3) } }
is run-method(C.new), 42, 'a method the module calls';
subtest 'a subtest still counts', { plan 1; ok 1, 'inside' };

{
    sub any(*@values) { 'mine' }
    ok module-any(), "a block's any does not reach the module";
    is any(1), 'mine', 'the block sees its own any';
}
ok module-any(), 'nor after the block';
