unit module OpenSSL::Version;

use NativeCall;
use OpenSSL::NativeLib;

our int32 constant VERSION     = 0;
our int32 constant CFLAGS      = 1;
our int32 constant BUILT_ON    = 2;
our int32 constant PLATFORM    = 3;
our int32 constant DIR         = 4;
our int32 constant ENGINES_DIR = 5;

our sub version_num returns Int {
    my sub OpenSSL_version_num returns ulong is native(&gen-lib) { ... }
    try { OpenSSL_version_num() } // 0;
}

our sub version(int32 $type = VERSION) returns Str {
    my sub OpenSSL_version(int32) returns Str is native(&gen-lib) { ... }
    try { OpenSSL_version($type) } // '';
}

# vim: expandtab shiftwidth=4
