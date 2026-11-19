use Test;
use lib $?FILE.IO.parent(2).add('lib').Str;

# A package-block `my %seen` / `my @log` written by one of the package's
# routines from inside a closure. Once another class that imports the
# routine is `require`d at run time from a frame binding a same-named
# variable of its own, the closure's capture held that other `%seen`; the
# element store went into it while every read answered from the package
# block's own container, so the write was lost (PDF::COS::Util's cycle
# detection never fired and t/io-serialize.t overflowed the stack).
# Every expectation was checked against rakudo.

use RequiredCaptureSeen::Dict;

plan 4;

my $d = RequiredCaptureSeen::Dict.new;
is $d.note('a'), '1/1', 'the first store lands in the package-block containers';
is $d.note('a'), '1/2', 'a repeated key finds the stored element';
is $d.note('b'), '2/3', 'a second key is added to the same containers';
is $d.own, '1/1', "the caller's same-named containers are untouched";
