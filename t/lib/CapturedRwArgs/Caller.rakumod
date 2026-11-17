# Fixture for t/routines/dispatch/captured-multi-dispatcher-rw-argument.t: calls
# a multi it cannot see by name, handed in as a code object.
unit module CapturedRwArgs::Caller;
sub call-with(&op, $left) is export {
    my $right = 2;
    op($left, $right)
}
sub call-literal(&op, $left) is export { op($left, 3) }
