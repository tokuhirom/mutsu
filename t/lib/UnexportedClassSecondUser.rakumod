unit module UnexportedClassSecondUser;
use UnexportedClassUnit;

sub second-user-hidden() is export {
    my $result = try EVAL 'Hidden.value';
    $! ?? $!.^name !! $result
}

sub second-user-public() is export { Public.value }
