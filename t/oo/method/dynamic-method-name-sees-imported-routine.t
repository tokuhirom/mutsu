use v6;
use Test;
use lib 't/lib';
use DynMethodHelper;

# A method with a computed name (`method ::('name') { ... }`) is compiled when
# the class registers, and its body must still see the routines the file's
# `use` bound. It used to die with "Unknown function: dyn-helper" while an
# ordinary method of the same class worked. Seen in the JSON::RPC dist, whose
# client declares `method ::('rpc.flush')` and calls the imported `to-json`.

plan 3;

class C {
    method ::('dyn') { dyn-helper(1) }
    method ::('with.dot') { dyn-helper(2) }
    method plain { dyn-helper(3) }
}

is C.new.'dyn'(), 'helped(1)', 'a computed-name method calls an imported sub';
is C.new.'with.dot'(), 'helped(2)', 'also when the name is not an identifier';
is C.new.plain, 'helped(3)', 'an ordinary method still works';
