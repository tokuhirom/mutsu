use Test;
use NativeCall;

plan 3;

# A native (`is native`) sub called as a bare STATEMENT -- its return value
# sunk, not assigned or used -- compiled to the `ExecCall` opcode, which never
# checked the NativeCall dispatch table (only the `CallFunc` opcode, used for
# expression-context calls, did). So a call like `sqlite3_extended_result_codes
# ($p, 1);` (DBIish's SQLite driver, no assignment) ran its literal `{ ... }`
# stub body instead of the real C call and died with "Stub code executed".
# Found while bundling the DBIish battery.

sub setenv(Str, Str, int32 --> int32) is native { * }
sub getenv(Str --> Str) is native { * }

# Call it as a bare statement (sunk context) -- this is the shape that
# regressed. Its return value (0 on success) is discarded.
setenv('MUTSU_NATIVECALL_SUNK_TEST', 'ok', 1);
is getenv('MUTSU_NATIVECALL_SUNK_TEST'), 'ok',
    'a native sub called as a bare (sunk) statement really dispatches over FFI';

# The same call in expression context (assigned) always worked; pin it too so
# a future change can't fix one path while breaking the other.
my $rc = setenv('MUTSU_NATIVECALL_SUNK_TEST2', 'also-ok', 1);
is $rc, 0, 'the same native sub in expression context returns the real C result';
is getenv('MUTSU_NATIVECALL_SUNK_TEST2'), 'also-ok',
    'and it really ran the C call, not the sunk-context stub body';
