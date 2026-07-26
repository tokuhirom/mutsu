use v6;
use Test;

plan 6;

# `$*VM.osname` exposes the build-time OS name (zef branches on it).
ok $*VM.osname.defined, '$*VM.osname is defined';
isa-ok $*VM.osname, Str, '$*VM.osname is a Str';
ok $*VM.osname.chars > 0, '$*VM.osname is non-empty';

# `$*VM.config<nativecall_backend>` names the FFI implementation. `NativeLibs`
# reads it unconditionally, so an undefined value there warns on every load.
ok $*VM.config<nativecall_backend>.defined, '$*VM.config<nativecall_backend> is defined';
isa-ok $*VM.config<nativecall_backend>, Str, '$*VM.config<nativecall_backend> is a Str';
ok $*VM.config<be>.defined, '$*VM.config<be> is still defined';
