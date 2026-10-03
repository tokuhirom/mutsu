# `PROCESS::<$OUT>` is the process-level dynamic: a frame's own `my $*OUT`
# shadows `$*OUT` lookups but not the PROCESS stash. Found via Tee, whose
# constructor reads `PROCESS::<$OUT>` while its caller holds `my $*OUT`.
use Test;

plan 6;

my $process-out = PROCESS::<$OUT>;
isa-ok $process-out, IO::Handle, 'PROCESS::<$OUT> is the process handle';
{
    my $*OUT = 42;
    is $*OUT, 42, 'my $*OUT shadows the dynamic lookup';
    ok PROCESS::<$OUT> === $process-out, 'but not PROCESS::<$OUT>';
    sub read-process { PROCESS::<$OUT> }
    ok read-process() === $process-out, 'nor PROCESS::<$OUT> read from a callee';
}

PROCESS::<$PROCESS-STASH-TEST> = 7;
{
    my $*PROCESS-STASH-TEST = 8;
    is PROCESS::<$PROCESS-STASH-TEST>, 7, 'a PROCESS:: install is not shadowed by a lexical one';
}

my $err = $*ERR;
{
    temp $*OUT = $err;
    ok PROCESS::<$OUT> === $err, 'assigning the dynamic without declaring it sets the process value';
}
