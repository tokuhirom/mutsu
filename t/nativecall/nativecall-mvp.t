use Test;

# Minimal NativeCall (C FFI) MVP: a sub declared `is native(...)` with a `{ * }`
# body is dispatched through C FFI (dlopen + libffi) instead of running Raku
# code. Exercises scalar int / num marshalling, Str -> char*, the `is symbol`
# override, and a non-default library ("m").

use NativeCall;

plan 7;

sub c_abs(int32 $n) returns int32 is native('c') is symbol('abs') { * }
sub c_strlen(Str $s) returns int64 is native('c') is symbol('strlen') { * }
sub c_toupper(int32 $c) returns int32 is native('c') is symbol('toupper') { * }
sub c_getpid() returns int32 is native('c') is symbol('getpid') { * }
sub c_pow(num64 $base, num64 $exp) returns num64 is native('m') is symbol('pow') { * }
sub c_sqrt(num64 $x) returns num64 is native('m') is symbol('sqrt') { * }

is c_abs(-5),       5,   'int32 -> int32 (abs)';
is c_strlen('hello'), 5, 'Str -> char* -> int64 (strlen)';
is c_toupper(97),   65,  'int32 -> int32 (toupper of "a" is "A")';
ok c_getpid() > 0,       'no-arg int32 return (getpid is positive)';
is c_pow(2e0, 10e0), 1024e0, 'two num64 args -> num64 (pow), non-c library';
is c_sqrt(16e0),    4e0,  'num64 -> num64 (sqrt)';

# A bad symbol surfaces as a clear runtime error, not a crash.
sub c_nope() returns int32 is native('c') is symbol('this_symbol_does_not_exist_xyz') { * }
dies-ok { c_nope() }, 'calling a missing symbol dies cleanly';
