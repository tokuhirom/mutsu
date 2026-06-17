use Test;

# Regression (Slice C', open-question #2): a fully-reconciled bareword/EVAL
# carrier drops its blanket `env_dirty` net (#3227/#3231). The `$x ~~ s///`
# writeback inserts the mutated value into `env` directly (bypassing
# `set_env_with_main_alias`) and only updates the *current* frame's slot — so
# when the substitution runs inside EVAL, the outer `$x` is written to env but
# never logged into the carrier write set, and the dropped net loses it. The
# writeback now self-reports via `note_caller_env_write`, so the carrier-return
# reconcile sees `$x` and keeps it coherent.

plan 6;

# 1. the core case: EVAL'd `$x ~~ s///` must mutate the outer lexical
{
    my $x = "abc";
    EVAL '$x ~~ s/ab/xy/';
    is $x, "xyc", 'EVAL\'d $x ~~ s/// mutates the outer lexical';
}

# 2. no match leaves the value untouched
{
    my $x = "abc";
    EVAL '$x ~~ s/zz/qq/';
    is $x, "abc", 'EVAL\'d non-matching s/// leaves the value';
}

# 3. global substitution through EVAL
{
    my $x = "a-a-a";
    EVAL '$x ~~ s:g/a/b/';
    is $x, "b-b-b", 'EVAL\'d s:g/// mutates all occurrences';
}

# 4. repeated sequential EVAL'd substitutions accumulate on the outer lexical
{
    my $x = "0000";
    EVAL '$x ~~ s/0/1/';
    EVAL '$x ~~ s/0/1/';
    EVAL '$x ~~ s/0/1/';
    is $x, "1110", 'repeated EVAL\'d s/// accumulate on the outer lexical';
}

# 5. the writeback still works at top level (no carrier) — no regression
{
    my $x = "abc";
    $x ~~ s/ab/xy/;
    is $x, "xyc", 'top-level $x ~~ s/// still mutates (no regression)';
}

# 6. an unrelated hot local stays coherent across the EVAL carrier
{
    my $x = "abc";
    my $acc = 41;
    EVAL '$x ~~ s/a/A/';
    $acc = $acc + 1;
    is "$x/$acc", "Abc/42", 'EVAL\'d s/// + later local write both correct';
}
