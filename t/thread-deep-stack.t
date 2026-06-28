use v6;
use Test;

# Regression: threads spawned by `Thread.start` must use the same large
# user-code stack as `start {}` / Promise / Supply worker threads. Previously
# `Thread.start` used the OS default (~2-8 MiB) stack, so deep VM nesting on the
# worker thread overflowed and aborted the whole process ("has overflowed its
# stack"). This surfaced when running an async web server (HTTP::Server::Tiny)
# whose react loop constructs objects whose BUILD re-enters the VM.
#
# Here we force deep recursion on a `Thread.start` worker; depth ~600 frames overflow
# a default thread stack but fits comfortably in the 256 MiB user-code stack.

plan 2;

sub deep(Int $n) {
    return 0 if $n <= 0;
    return 1 + deep($n - 1);
}

# Baseline: deep recursion works on the main thread.
is deep(600), 600, 'deep recursion on main thread';

# The regression: the same work on a Thread.start worker must not overflow.
my $result;
my $thr = Thread.start({
    $result = deep(600);
});
$thr.finish;
is $result, 600, 'deep recursion on a Thread.start worker thread';
