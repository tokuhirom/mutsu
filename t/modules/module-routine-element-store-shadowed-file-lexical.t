use Test;
use lib $?FILE.IO.parent(2).add('lib').Str;

# A module routine's own `my @log` / `my %seen` shadows the module's
# file-scope variable of the same name: an element store or a key store
# through the routine's variable goes to the routine's container, not to the
# file's (nor nowhere). Every expectation was checked against rakudo.

use ShadowedFileLexicals;

plan 6;

is push-own(), '2/2', 'a push to the routine-local array';
is key-own(), 1, 'a key store to the routine-local hash';
is index-own(), 1, 'an element store to the routine-local array';
is typed-key-own('k'), 'k', 'a key store to a routine-local typed hash';
is file-sizes(), '0/0', "the file-scope containers stayed empty";
is file-store(), '1/1', 'a routine without its own `my` still stores to the file scope';
