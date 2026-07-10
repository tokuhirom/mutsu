use v6;
use Test;

plan 3;

# `$*VM.osname` exposes the build-time OS name (zef branches on it).
ok $*VM.osname.defined, '$*VM.osname is defined';
isa-ok $*VM.osname, Str, '$*VM.osname is a Str';
ok $*VM.osname.chars > 0, '$*VM.osname is non-empty';
