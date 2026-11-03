use v6;
use lib 't/lib/ResSplit/lib';
use Test;

plan 4;

# From Sort::Fast (via NativeCall's `guess_library_name`): a `%?RESOURCES` entry
# answers `.platform-library-name`, the IO::Path of its library file.
use ResSplit;

my $r = strs();
isa-ok $r.platform-library-name, IO::Path, 'an IO::Path';
is $r.platform-library-name.Str, $r.IO.Str, 'it is the resource file';
is $r.platform-library-name.basename, 'strs.bin', 'the resolved file name';
throws-like { 'x'.IO.platform-library-name }, X::Method::NotFound, 'a plain IO::Path has no such method';
