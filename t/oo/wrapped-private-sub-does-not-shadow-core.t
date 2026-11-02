use lib 't/lib';
use Test;
use WrappedPrivateOpen;

plan 3;

# The module's `sub open` is private and wrapped. The script's `open` is still
# the core routine, and the module's own call still reaches its wrapped sub.
my $fh = open "t/oo/wrapped-private-sub-does-not-shadow-core.t", :r;
is $fh.^name, 'IO::Handle', 'a private wrapped sub of a used module does not shadow core open';
$fh.close;
is WrappedPrivateOpen.hi, 'mine', "the module's own call still reaches its private sub";

sub close(Int $x) { "local $x" }
&close.wrap(-> |c { callsame });
is close(3), 'local 3', "a script's own wrapped sub is still called";
