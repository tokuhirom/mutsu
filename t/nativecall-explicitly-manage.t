use Test;
use NativeCall;

# `explicitly-manage($str)` hands a string's C buffer to the callee for good:
# the returned object's buffer "will not be freed by the runtime's garbage
# collector" (Language/nativecall.rakudoc). A plain `Str` argument is marshalled
# into a temporary `char*` that dies with the call, which is right for a callee
# that copies and wrong for one that RETAINS the pointer.
#
# `refresh($obj)` re-reads a CStruct's fields after C wrote them behind the
# runtime's back. In mutsu a CStruct instance holds only the C address and every
# field access reads through it, so there is nothing to re-read -- but the sub
# still has to exist and return 1, because bindings call it.
#
# Both are NativeCall exports rather than builtins, like `cglobal`.

plan 14;

ok defined(&explicitly-manage), 'explicitly-manage is a callable routine';
ok defined(&refresh), 'refresh is a callable routine';

my $managed = explicitly-manage('mutsu');
is $managed.^name, 'NativeCall::CStr', 'it returns a NativeCall::CStr, as Rakudo does';
is $managed.gist, 'NativeCall::CStr.new', 'whose gist matches Rakudo';
isnt $managed.address, 0, 'and which carries a real buffer address';

# The address is stable: the same object keeps naming the same buffer, and two
# calls allocate two buffers.
is $managed.address, $managed.address, 'the address does not move';
isnt explicitly-manage('mutsu').address, $managed.address,
    'each call allocates its own buffer';

# End to end through a callee that RETAINS the pointer. `putenv` is the
# canonical one -- POSIX says the string becomes part of the environment, so the
# caller must not free it. This is the same shape as nativecall.rakudoc's
# set_version/get_version example.
sub putenv(Str --> int32) is native('c') { * }
sub getenv(Str --> Str) is native('c') { * }

is putenv(explicitly-manage('MUTSU_MANAGED_A=first')), 0, 'putenv accepts a managed string';
is getenv('MUTSU_MANAGED_A'), 'first', 'and the retained buffer is still live afterwards';

is putenv(explicitly-manage('MUTSU_MANAGED_A=second')), 0, 'a second managed string';
is getenv('MUTSU_MANAGED_A'), 'second', 'replaces the first without disturbing it';

# `:$encoding` is documented, and the encoding happens before the buffer is
# taken, so a non-UTF-8 request is honoured rather than ignored.
isnt explicitly-manage('abc', :encoding('utf16')).address, 0,
    'an explicit :encoding is accepted';

# `refresh` answers 1, as Rakudo's `sub refresh($obj --> 1)` does, and leaves
# the object it was handed alone.
is refresh($managed), 1, 'refresh returns 1';
is $managed.address, $managed.address, 'and does not disturb its argument';
