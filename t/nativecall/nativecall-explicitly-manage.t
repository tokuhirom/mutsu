use Test;
use NativeCall;
use nqp;

# `explicitly-manage($str)` hands a string's C buffer to the callee for good:
# the result's buffer "will not be freed by the runtime's garbage collector"
# (Language/nativecall.rakudoc). A plain `Str` argument is marshalled into a
# temporary `char*` that dies with the call, which is right for a callee that
# copies and wrong for one that RETAINS the pointer.
#
# `refresh($obj)` is `nqp::nativecallrefresh`, which re-reads a CStruct's
# fields after C wrote them behind the runtime's back; it answers 1.
#
# Both are upstream NativeCall's own routines (vendored, run verbatim). What
# `explicitly-manage` returns differs between Rakudo releases -- the `CStr`
# object itself (2026.06), or the `Str` it was given with `.cstr` set
# (2026.09) -- so the tests reach the buffer through `.cstr` when there is one.
#
# Every expectation was verified against Rakudo.

plan 14;

ok defined(&explicitly-manage), 'explicitly-manage is a callable routine';
ok defined(&refresh), 'refresh is a callable routine';

sub buffer-of($managed) { $managed ~~ Str ?? $managed.cstr !! $managed }

my $managed = explicitly-manage('mutsu');
my $cstr = buffer-of($managed);
is $cstr.REPR, 'CStr', 'the managed buffer is a CStr-REPR object';
is nqp::unbox_s($cstr), 'mutsu', 'which holds the string';
ok $cstr.defined, 'and is a defined object';

my $other = buffer-of(explicitly-manage('mutsu'));
is nqp::unbox_s($other), 'mutsu', 'a second call holds the same string';
isnt $other.WHERE, $cstr.WHERE, 'in an object of its own';

# End to end through a callee that RETAINS the pointer. `putenv` is the
# canonical one -- POSIX says the string becomes part of the environment, so the
# caller must not free it. This is the same shape as nativecall.rakudoc's
# set_version/get_version example.
sub putenv(Str --> int32) is native('c', v6) { * }
sub getenv(Str --> Str) is native('c', v6) { * }

is putenv(explicitly-manage('MUTSU_MANAGED_A=first')), 0, 'putenv accepts a managed string';
is getenv('MUTSU_MANAGED_A'), 'first', 'and the retained buffer is still live afterwards';

is putenv(explicitly-manage('MUTSU_MANAGED_A=second')), 0, 'a second managed string';
is getenv('MUTSU_MANAGED_A'), 'second', 'replaces the first without disturbing it';

# `:$encoding` is accepted.
lives-ok { explicitly-manage('abc', :encoding('utf16')) }, 'an explicit :encoding is accepted';

# `refresh` answers 1 and leaves the object it was handed alone.
is refresh($managed), 1, 'refresh returns 1';
is nqp::unbox_s(buffer-of($managed)), 'mutsu', 'and does not disturb its argument';
