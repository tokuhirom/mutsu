use lib 't/lib';
use Test;
use CapMultiHolder;
use CapMultiStatics :mark;

plan 7;

my $d = CapMultiHolder.new; $d<a> = 1;
is $d.content, 1, "a module multi reads its own %seen, not the calling method's class static";
my $e = CapMultiHolder.new; $e<b> = 1;
is $e.content, 2, "... and keeps what it stored";
is count(), 2, "the module's plain sub sees the same hash";

is $d.bump(5), 5, "a module multi writes its own scalar static, not the calling class's";
is $e.bump(2), 7, "... accumulating across calls";
is total(), 7, "the module sees the total";
is $d.own-total, 100, "the calling class's own static is untouched";
