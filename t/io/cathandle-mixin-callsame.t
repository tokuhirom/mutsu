use Test;

# `callsame` out of a role mixed into an explicitly constructed IO::CatHandle
# reaches the native IO::CatHandle method (#15825, seen with LN's
# IO::CatHandle::AutoLines).

plan 4;

my $file = $*TMPDIR.add("mutsu-cathandle-mixin-{$*PID}.txt");
$file.spurt("one\ntwo\n");
LEAVE $file.unlink;

role Count { has $.ln is rw = 0; method get { my \v = callsame; ++$!ln; v } }

my $h = IO::CatHandle.new($file);
$h does Count;
is $h.get, 'one', 'callsame reaches the native get';
is $h.ln, 1, 'the role attribute updated';
is $h.get, 'two', 'second line';
is $h.ln, 2, 'counter keeps counting';
