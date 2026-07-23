unit module OpenSSL::Err;

use OpenSSL::NativeLib;
use NativeCall;

our sub ERR_error_string(int32 $e, Str $ret --> Str) is native(&gen-lib) { ... }

our sub ERR_get_error(--> ulonglong) is native(&gen-lib) { ... }

# vim: expandtab shiftwidth=4
