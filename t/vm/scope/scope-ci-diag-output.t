use Test;

# TEMPORARY (#19025): CI-only failures of 16 t/ files that pass locally.
# Captures their full output into $MUTSU_CRASH_DIR, which the crash-report
# step prints into the job log. To be reverted once the cause is known.

plan 1;

my $dir = %*ENV<MUTSU_CRASH_DIR>;
if $dir {
    mkdir $dir;
    for <
        t/oo/class/classify-list-plain-hash-keys.t
        t/routines/signature/entry-bound-var-to-raw-closure-param.t
        t/collections/array/array-mutator-rows.t
    > -> $f {
        my $p = run $*EXECUTABLE, $f, :out, :err;
        my $out = $p.out.slurp(:close);
        my $err = $p.err.slurp(:close);
        spurt "$dir/diag-{$f.IO.basename}.txt",
            "=== $f exit {$p.exitcode}\n--- stdout\n$out\n--- stderr\n$err\n";
    }
}
pass 'diagnostics captured';
