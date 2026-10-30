use Test;
use NativeCall;

# Shell::DSL `run-pipeline`: `my $fd := @pipes[i][j]; c_close($fd)`. A CArray
# element bound to a variable is an `IntPosRef`; the native call marshalled it
# as 0, so `close(0)` ran instead of `close(3)` and the pipe's write end stayed
# open in the parent (the reader never saw EOF).

plan 6;

sub c_abs(int32 $x --> int32) is native is symbol('abs') { * }
sub c_pipe(CArray[int32] --> int32) is native is symbol('pipe') { * }
sub c_close(int32 --> int32) is native is symbol('close') { * }

my $a = CArray[int32].new(-5, -6);
my $bound := $a[1];
is c_abs($bound), 6, 'a bound CArray element is passed as the element it addresses';
is c_abs($a[0]), 5, 'as is an unbound element';

my @arrays;
@arrays[0] := CArray[int32].new(-7, -8);
my $in-array := @arrays[0][1];
is c_abs($in-array), 8, 'a bound element of an array-held CArray';

$a[1] = -9;
is c_abs($bound), 9, 'the binding follows a later store';

my $p = CArray[int32].new(0, 0);
is c_pipe($p), 0, 'pipe(2) fills the CArray';
my $read := $p[0];
my $write := $p[1];
is (c_close($read), c_close($write)), (0, 0), 'closing through the bound elements closes the real fds';
