# A thread spawned BEFORE the first test call must share the TAP counter.
# The first `ok` of the whole program runs inside a supply tap driven by a
# `start` block (the Cro HTTP/2 serializer-test shape). Before the fix, the
# spawned thread lazily created a private TestState: its increments never
# reached the main thread, main restarted numbering at 1, and prove failed
# the file with "Tests out of sequence" even though every assertion passed.
use Test;

plan 6;

my $s = Supplier.new;
my $done = Promise.new;
$s.Supply.tap: -> $v {
    ok True, "in tap $v";
    $done.keep if $v == 2;
};
start { $s.emit(1); $s.emit(2); }
await Promise.anyof($done, Promise.in(5));
ok True, "main after first";

my $s2 = Supplier.new;
my $done2 = Promise.new;
$s2.Supply.tap: -> $v {
    ok True, "in tap2 $v";
    $done2.keep if $v == 2;
};
start { $s2.emit(1); $s2.emit(2); }
await Promise.anyof($done2, Promise.in(5));
ok True, "main after second";
