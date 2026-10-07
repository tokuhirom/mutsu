use lib 't/lib';
use Test;

# NativeCall's `sub EXPORT` hands its importers `&trait_mod:<is>` as the
# dispatcher of one `multi`. When a module has already loaded NativeCall (and
# exports `trait_mod:<is>` candidates of its own), the importer's own
# `use NativeCall` must still bring the `is native` candidate, and the module's
# trait must keep working beside it.
#
# Every expectation was verified against Rakudo.

plan 6;

use NativeCallNestedUser;
use NativeCall;

ok nested-pid() > 0, 'the module calls its own native routine';

sub my-pid(--> int32) is native('c', v6) is symbol('getpid') { * }
ok my-pid() > 0, 'the importer declares one too';
is my-pid(), nested-pid(), 'both reach the same C function';

sub tagged is nested-tag { 5 }
is tagged(), 'nested<5>', 'the module\'s trait applies';

sub both(--> int32) is native('c', v6) is symbol('getpid') is nested-tag { * }
like both(), /^ 'nested<' \d+ '>' $/, 'a routine can carry is native and the module trait';

throws-like { EVAL 'sub unknown is no-such-trait { 1 }; 1' },
    Exception, message => /'unknown trait'/,
    'an unknown trait is still an error with both loaded';
