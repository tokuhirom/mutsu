use v6;
use Test;

# `dir` and `IO::Path.dir` are one body, and the file tests `.r`/`.rw`/...
# and `$path ~~ :r` are one body (ADR-0118 §2.7). Each pair had its own
# copy: the method returned a List where the sub returned a Seq, both died
# with an X::AdHoc instead of X::IO::Dir, and the smartmatch tested mode
# bits (not access(2)) against a path it never resolved against the
# IO::Path's CWD. Expected values were measured with rakudo.

plan 17;

my $root = $*TMPDIR.add("mutsu-dir-one-body-{$*PID}");
$root.mkdir;
$root.add("a.txt").spurt("hi");
$root.add("sub").mkdir;
$root.add("zero").spurt("");

isa-ok $root.dir, Seq, 'IO::Path.dir is a Seq (it was a List)';
isa-ok dir($root), Seq, '... as dir() is';
is $root.dir.map(*.basename).sort.join(','), 'a.txt,sub,zero', '.dir lists the entries';
is $root.dir(test => *).map(*.basename).sort.join(','), '.,..,a.txt,sub,zero',
    '.dir(:test) sees . and ..';
is $root.dir(test => /a/).map(*.basename).join(','), 'a.txt', '.dir(:test(Regex))';

my $nope = $root.add("nope");
throws-like { $nope.dir }, X::IO::Dir, '.dir of a missing directory throws X::IO::Dir';
throws-like { dir $nope }, X::IO::Dir, '... as dir() does';
try { sink $nope.dir }
ok $!.message.starts-with("Failed to get the directory contents of '"), '... with rakudo\'s message';
ok $!.message.ends-with("': Failed to open dir: no such file or directory"), '... and reason';
throws-like { $root.add("a.txt").dir }, X::IO::Dir, '.dir of a file throws';

# The file tests: the smartmatch is the method.
my $z = $root.add("zero");
is ($z ~~ :r), $z.r, '~~ :r agrees with .r';
is ($z ~~ :rw), $z.rw, '~~ :rw agrees with .rw';
is ($z ~~ :rwx), $z.rwx, '~~ :rwx agrees with .rwx';
my $rel = IO::Path.new("a.txt", :CWD($root.absolute));
ok $rel ~~ :s, '~~ :s resolves against the IO::Path CWD (it tested the bare name)';
ok $rel ~~ :rw, '~~ :rw resolves against the CWD too';
nok $nope ~~ :r, '~~ :r of a missing path is False';
ok $nope ~~ :!e, '~~ :!e of a missing path is True';

.d ?? .rmdir !! .unlink for $root.dir;
$root.rmdir;
