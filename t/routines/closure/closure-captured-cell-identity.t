use v6;
use Test;

# A closure call re-installs the box-on-capture cells it captured over whatever
# the calling frame holds (#7565). When the frame already resolves a name to the
# very same cell the install is skipped; every case where the frame holds a
# different value must still see the closure's own cell.

plan 8;

our $shared = 1;
my $counter = 0;

sub make-bump() {
    return sub { $counter++; $shared++; $counter + $shared };
}
my &bump = make-bump();
is bump(), 3, 'a closure reads and writes the captured lexical and the our variable';
is $counter, 1, 'the captured lexical was written through the shared cell';
is $shared, 2, 'the our variable was written through the shared cell';

$shared = 10;
$counter = 20;
is bump(), 32, 'a later call sees the values the caller stored in between';

sub call-it(&f) { my $counter = 100; f() }
is call-it(&bump), 34, 'a caller-local of the same name does not hide the closure\'s cell';
is $counter, 22, 'and the closure wrote the captured cell, not the caller local';

my @subs;
for 1..3 -> $i {
    my $acc = $i * 10;
    @subs.push({ $acc += $i; $acc });
}
is-deeply @subs.map({ $_() }).List, (11, 22, 33), 'each closure keeps its own captured cell';
is-deeply @subs.map({ $_() }).List, (12, 24, 36), 'and keeps mutating it on later calls';
