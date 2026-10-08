use Test;

# TEMPORARY diagnostic for #9930 (CI-only failure); removed before merge.
%*ENV<MUTSU_BIN> = ~$*EXECUTABLE;
%*ENV<MUTSU_T_TIMEOUT> = '45';
my @report;
for <seq-array-context-reiterate seq-skip-consumed seq-consumption-matrix> -> $name {
    my $p = run 'prove', '--merge', '-v', '-e', 'scripts/run-t-test.sh',
        "t/collections/lazy-seq/$name.t", :out, :err;
    my $out = $p.out.slurp(:close);
    my $err = $p.err.slurp(:close);
    my $txt = ($out ~ $err).lines.grep({ !/^ok / && !/^\s+ok / }).join(' | ');
    @report.push("$name rc={$p.exitcode} [$txt]");
}
say "Bail out! " ~ @report.join(' ## ');
