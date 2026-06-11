use Test;

# VM-native printf (File+UTF8) and flush (any target) on an IO::Handle
# (③ native IO PR-D Tier-2b). printf builds its payload via the pure sprintf
# helpers and writes through the shared File path; flush is target-agnostic and
# pure. Must behave identically to the interpreter's native fork.

plan 9;

my $path = $*TMPDIR.child("mutsu-io-t2b-{$*PID}.txt");

# printf basic + multiple directives + width/precision
{
    my $fh = $path.open(:w);
    $fh.printf("%d-%s|", 42, "x");
    $fh.printf("%05.2f", 3.14159);
    $fh.close;
    is $path.slurp, "42-x|03.14", 'printf formats and writes to the file';
}

# printf return value is True
{
    my $fh = $path.open(:w);
    ok $fh.printf("%s", "ok") === True, 'printf returns True';
    $fh.close;
}

# a malformed printf directive throws (too few args)
{
    my $fh = $path.open(:w);
    dies-ok { $fh.printf("%d %d", 1) }, 'printf with too few args dies';
    $fh.close;
}

# flush returns True and the buffered data is persisted
{
    my $fh = $path.open(:w);
    $fh.out-buffer = 1024;
    $fh.print("buffered");
    ok $fh.flush === True, 'flush returns True';
    # after an explicit flush the bytes are on disk even before close
    is slurp($path), "buffered", 'flush persists buffered bytes';
    $fh.close;
}

# flush is idempotent
{
    my $fh = $path.open(:w);
    $fh.print("x");
    $fh.flush;
    ok $fh.flush === True, 'a second flush also returns True';
    $fh.close;
}

# flush works on the standard handles (universal, not File-gated)
{
    lives-ok { $*OUT.flush }, '$*OUT.flush lives';
    lives-ok { $*ERR.flush }, '$*ERR.flush lives';
}

# printf honoring the handle encoding still works for UTF-8 multibyte
{
    my $fh = $path.open(:w);
    $fh.printf("%s", "café");
    $fh.close;
    is $path.slurp, "café", 'printf writes UTF-8 multibyte correctly';
}

$path.unlink;
