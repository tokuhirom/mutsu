use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

plan 4;

my $d = make-temp-dir();
$d.add("f$_").spurt("") for ^101;

ok dir($d).elems > 100, 'dir lists more than 100 entries';
ok dir($d).gist.ends-with("...)"), "dir's Seq gist is truncated with ...";
ok dir($d).List.gist.ends-with("...)"), "dir's List gist is truncated with ...";
is dir($d).gist.words.elems, 101, 'gist shows 100 entries plus the ... marker';
