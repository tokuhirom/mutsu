use lib 't/fixtures/export-native-rw/lib';
use Test;
use ExportNativeRw;
plan 3;

# #12521: a native routine an `EXPORT` hook installs as `&sub` writes its
# `is rw` out-parameter back to a sub-local variable, as to a file-scope one.
sub local-var { my int32 $e = 0; frexp(8e0, $e); $e }
is local-var(), 4, 'native out-param written to a sub-local variable';
my int32 $g = 0;
frexp(8e0, $g);
is $g, 4, 'native out-param written to a file-scope variable';
sub local-plain { my uint32 $l = 1; pl($l); $l }
is local-plain(), 5, 'plain exported is rw sub still writes back';
