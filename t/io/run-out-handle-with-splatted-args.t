use Test;

# Found via the `static` distribution (t/14-cli.rakutest): `run` with an
# `:out($handle)` / `:err($handle)` redirect lost the child's output whenever
# the same call also splatted a list into its arguments (`|@include`), because
# the handle variable reached the option reader still wrapped in its container.

plan 6;

my $f = $*TMPDIR.add("run-out-handle-splat-{$*PID}");
my @none = ();
my @some = <a b>;

sub captured(&call) {
    my $o = $f.open(:w);
    call($o);
    $o.close;
    $f.slurp;
}

is captured({ run 'echo', 'x', :out($_) }), "x\n", 'plain argument list';
is captured({ run 'echo', |@none, 'x', :out($_) }), "x\n", 'empty splat before positionals';
is captured({ run 'echo', 'x', |@none, :out($_) }), "x\n", 'empty splat after positionals';
is captured({ run 'echo', |@some, 'x', :out($_) }), "a b x\n", 'non-empty splat';

my $e = $f.open(:w);
run 'sh', '-c', 'echo oops >&2', |@none, :err($e);
$e.close;
is $f.slurp, "oops\n", ':err handle with a splat';

my $o = $f.open(:w);
run 'sh', '-c', 'echo both; echo oops >&2', |@none, :out($o), :err($o);
$o.close;
is $f.slurp.lines.sort.join(','), 'both,oops', ':out and :err to one handle with a splat';

$f.unlink;
