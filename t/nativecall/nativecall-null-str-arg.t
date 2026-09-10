use Test;
use NativeCall;

# An UNDEFINED `Str` argument to a native sub must be passed as a NULL `char*`,
# as in Rakudo. Stringifying it instead handed C a pointer to a 1-byte buffer;
# a callee that treats the parameter as a caller-supplied OUTPUT buffer then
# wrote past it and corrupted the heap. The real-world case is OpenSSL's
# `ERR_error_string($e, Nil)`, where NULL means "use your own static buffer":
# it writes up to 256 bytes and aborted mutsu with
# "realloc(): invalid next size".
#
# `getcwd(char *buf, size_t size)` pins it precisely, and safely:
#   getcwd(NULL, 0)      -> allocates and RETURNS the cwd  (defined)
#   getcwd(<non-NULL>,0) -> fails with EINVAL, returns NULL (undefined)
# So a NULL is observably different from the empty string the old code sent
# (`Nil.Str` is ""), and size 0 means nothing is ever written into the buffer.

plan 5;

sub getcwd(Str, size_t --> Str) is native { * }
sub strlen(Str --> size_t) is native { * }

# A DEFINED Str still marshals as a normal NUL-terminated C string.
is strlen('hello'), 5, 'a defined Str arg is passed NUL-terminated';
is strlen(''), 0, 'an empty (but defined) Str arg is still a valid C string';

# An UNDEFINED Str must be NULL.
ok getcwd(Nil, 0).defined, 'Nil Str arg is passed as NULL (callee takes its NULL path)';
ok getcwd(Str, 0).defined, 'Str type-object arg is passed as NULL too';

# ...and is genuinely distinct from the empty string the old code would send.
nok getcwd('', 0).defined, 'an empty Str is NOT NULL (callee takes its non-NULL path)';

# vim: expandtab shiftwidth=4 ft=perl6
