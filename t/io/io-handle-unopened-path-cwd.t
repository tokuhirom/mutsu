use v6;
use Test;

# A handle made with `IO::Handle.new(:path($p))` is not open until `.open`, and
# a relative `IO::Path` opens against its own `CWD` (mutsu#14906,
# `Distribution::Locally.content`).
plan 6;

my $dir = $*TMPDIR.add( 'handle-cwd-' ~ $*PID );
END { run 'rm', '-rf', ~$dir if $dir.e }
$dir.add('res').mkdir;
$dir.add('res/x.txt').spurt: "hello\n";

my $path = IO::Path.new( 'res/x.txt', :CWD( $dir.absolute ) );
my $h = IO::Handle.new( :$path );
nok $h.opened, 'a handle that was never opened is not open';
$h.open;
ok $h.opened, 'and is after .open';
is $h.slurp.chomp, 'hello', 'a relative path opens against the path\'s own CWD';
$h.close;
nok $h.opened, 'closed again';
is $path.open.slurp.chomp, 'hello', 'IO::Path.open honours the CWD too';
nok IO::Handle.new( :path( "$dir/nope".IO ) ).opened, 'a handle on a missing file is simply not open';
