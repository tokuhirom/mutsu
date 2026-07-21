use Test;

# A `start` block escapes its creating frame, so lexicals it captures AND
# mutates are boxed into shared ContainerRef cells. A *dynamic* variable
# (`$*x`) must NOT be treated that way: it is dynamic-scope, so its live value
# inside the thread is whatever the current dynamic frame set (e.g. `indir`
# binds `$*CWD` for its block), not the cell captured from the creator.
#
# Regression: `start indir $dir, { my $r = $*CWD.basename ne $want; $*CWD = 42; $r }`
# read the creator's outer `my $*CWD` instead of the indir-set path, because
# the captured ContainerRef cell overwrote the indir binding when the closure
# entered (roast S32-io/indir.t test 75, "failures due to race conditions").
# Only manifested when the block *assigned* to `$*CWD`.

plan 3;

# Core repro without any filesystem dependency: a dynamic override set inside
# the thread must be read live, even when the block also assigns to the var.
{
    my $inner = 'expected';
    my int $failures;
    $failures += [+] await flat ^200 .map: {
        my $*VAR = $_;
        my $prom = start {
            my $*VAR = $inner;          # dynamic override for this frame
            my $res = $*VAR ne $inner;  # should be False: read the override
            $*VAR = 'clobber';          # the assignment that used to break the read
            $res
        }
        $failures++ unless $*VAR eq $_;
        $prom
    }
    is $failures, 0, 'dynamic override in a mutating start block is read live';
}

# indir specifically. Use a directory that actually exists so `indir` succeeds
# regardless of CWD, then confirm the thread reads the indir-bound $*CWD (not
# the outer `my $*CWD`) even though the block assigns to $*CWD.
{
    my $dir = $*TMPDIR.absolute;
    my $want = $*TMPDIR.basename;
    my $*CWD = 'sentinel-outer';
    my int $failures;
    $failures += [+] await flat ^200 .map: {
        start indir $dir, {
            my $res = $*CWD.basename ne $want;   # False: read the indir path
            $*CWD = 'clobber';
            $res
        }
    }
    is $failures, 0, 'start+indir reads the indir-bound $*CWD despite the assignment';
}

# The outer dynamic var is unaffected after the thread completes.
{
    my $dir = $*TMPDIR.absolute;
    my $*CWD = 'sentinel-outer';
    await start indir $dir, { $*CWD = 'clobber' };
    is $*CWD, 'sentinel-outer', 'the outer $*CWD is unchanged by the thread';
}
