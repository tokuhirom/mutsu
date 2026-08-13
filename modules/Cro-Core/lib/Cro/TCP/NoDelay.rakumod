unit module Cro::TCP::NoDelay;

use NativeCall;

# Reduce per-socket cost for checking OS
INIT my $is-win = $*DISTRO.is-win;

constant TCP_NODELAY = 1;

sub setsockopt(int32 $sockfd, int32 $level, int32 $optname,
               CArray[int32] $optval, int32 $optlen --> int32) is native { * }

sub nodelay($socket) is export {
    # Not tested on Windows systems, so silently do nothing there
    return if $is-win;

    my $nd   = $socket.native-descriptor;
    my $on   = CArray[int32].new(1);
    my $size = nativesizeof(int32) * $on.elems;

    if setsockopt($nd, PROTO_TCP, TCP_NODELAY, $on, $size) {
        my $errno := cglobal(Str, 'errno', int32);
        die "Failed to set TCP_NODELAY option on socket #$nd; errno = $errno";
    }
}
