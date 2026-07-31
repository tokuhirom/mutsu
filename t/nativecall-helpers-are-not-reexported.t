# NativeCall's helper routines are spliced into every compunit that uses them
# (mutsu has no NativeCall.rakumod to import them from). A module that merely
# *uses* NativeCall must not therefore re-export them: raku leaves `nativecast`
# undeclared after `use NativeLibs`, and in mutsu the re-exported copy collided
# with the importer's own spliced copy as a hard X::Redeclaration — which is
# what made every DBIish SQLCipher test file die.
#
# The first two assertions are the regression proper and pass under rakudo too.
# The last two call through `is native('c')`, which rakudo cannot resolve on a
# host where `libc.so` is a linker script rather than a shared object, so only
# mutsu runs them.
use Test;
use NativeCall;
use lib $?FILE.IO.parent.add('lib').Str;

plan 4;

# Loading both is the regression: the second compunit brings its own copy of
# `nativecast`, which used to clash with the one leaked out of the first.
lives-ok { EVAL 'use NativeCallHelperUser; use NativeCallHelperPeer; 1' },
    'two modules that each use NativeCall load together';

use NativeCallHelperUser;
use NativeCallHelperPeer;

# The module's own export still arrives.
ok defined(&cast-through), 'the module exports its own routine';

# And it works: the helper is reachable from the module's body even though the
# module declares it in no package of its own.
sub c_getenv(Str --> Pointer) is native('c') is symbol('getenv') { * }
ok cast-through(Str, c_getenv('PATH')).chars > 0,
    'the module body reaches the helper it never declared';

ok NativeCallHelperPeer.new.cast(Str, c_getenv('PATH')).chars > 0,
    'and so does a method body in another compunit';
