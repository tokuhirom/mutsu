use Test;

# TEMPORARY diagnostic for #9930 (CI-only failure); removed before merge.
my @report;
for <seq-array-context-reiterate seq-skip-consumed seq-consumption-matrix> -> $name {
    my $p = run $*EXECUTABLE, "t/collections/lazy-seq/$name.t", :out, :err;
    my $err = $p.err.slurp(:close).subst("\n", " | ", :g);
    my $out = $p.out.slurp(:close).lines.grep(*.starts-with('not ok')).join(' ; ');
    @report.push("$name rc={$p.exitcode} err=[$err] notok=[$out]");
}
plan :skip-all(@report.join(" ## "));
