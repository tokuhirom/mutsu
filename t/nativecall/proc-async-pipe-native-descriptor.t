use v6.d;
use Test;
use NativeCall;

# `Proc::Async::Pipe.native-descriptor` answers the descriptor of the pipe end
# the parent reads; a stream nothing is tapped on is not read by the runtime,
# so the caller can read (and close) that descriptor itself (#15647).

plan 8;

sub c_read(int32, CArray[uint8], size_t --> ssize_t) is native is symbol('read') { * }
sub c_close(int32 --> int32) is native is symbol('close') { * }

my $p = Proc::Async.new('printf', 'hello\n');
my $pipe = $p.stdout;
my $fd-promise = $pipe.native-descriptor;
isa-ok $fd-promise, Promise, 'native-descriptor is a Promise';
my $started = $p.start;
my $fd = await $fd-promise;
ok $fd > 2, 'it carries a real descriptor, not stdout/stderr';

my $buf = CArray[uint8].new(0 xx 16);
my $got = '';
loop {
    my $n = c_read($fd, $buf, 16);
    last if $n <= 0;
    $got ~= Blob.new($buf[^$n]).decode;
}
is $got, "hello\n", 'the caller reads the child output from the descriptor';
is c_close($fd), 0, 'and closes it';
my $proc = await $started;
is $proc.exitcode, 0, 'the process exited cleanly';

# A tapped stream is still read by the runtime.
my $q = Proc::Async.new('printf', 'tapped\n');
my @out;
$q.stdout.tap: -> $chunk { @out.push: $chunk };
await $q.start;
is @out.join, "tapped\n", 'a tapped stream delivers its output';

# stderr gets its own descriptor.
my $r = Proc::Async.new('sh', '-c', 'printf err >&2');
my $efd = $r.stderr.native-descriptor;
my $rs = $r.start;
my $e = await $efd;
ok $e > 2, 'stderr has its own descriptor';
my $b2 = CArray[uint8].new(0 xx 8);
my $n2 = c_read($e, $b2, 8);
is $n2, 3, 'three bytes of stderr';
c_close($e);
await $rs;
