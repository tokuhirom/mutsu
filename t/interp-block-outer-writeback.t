use Test;

# A string-interpolation block `"{ ... }"` may mutate an OUTER (or `our`)
# variable, and that side effect must persist — while the block still isolates
# its OWN `my`/`state` declarations (they don't leak, even when shadowing an
# outer same-name or nested in an expression). Regression:
# integration/advent2012-day10.t, and the scope-isolating do-block writeback.

plan 9;

# outer scalar mutated directly inside the interp block -> persists
{
    my $p = "init";
    my $s = "[{ $p = "hi"; "X" }]";
    is $s, "[X]", "interp block value";
    is $p, "hi", "outer scalar mutation inside interp block persists";
}

# outer `our` mutated via a sub called from the interp block -> persists
{
    our $Prompt;
    sub ask($t) { $Prompt = $t; "Q" }
    my $s = "{ ask 'name?' }";
    is $s, "Q", "interp sub-call value";
    is $Prompt, "name?", "our var written by sub in interp block persists";
}

# inner `my` shadowing an outer same-name is isolated
{
    my $n = 1;
    is "$n {my $n = 2} $n", "1 2 1", "inner my shadow does not leak";
}

# inner `my` does not leak at all
{
    my $r = "{ my $z = 9; $z }";
    is $r, "9", "inner my block value";
    nok (try { $z }).defined, "inner my does not leak to outer scope";
}

# `state` declared inside an interp block in nested loops is per-instance
{
    my @arr;
    for ^2 { for ^2 { @arr.push("{ (state $a)++ }") } }
    is @arr.join(","), "0,0,0,0", "state var in interp block uses block scope";
}

# `:into(my %h)` hash declaration still survives (must NOT be isolated)
{
    my @data = <a b a c b a>;
    my %h := @data.classify({ $_ }, :into(my %dest));
    is %dest.keys.sort.join(","), "a,b,c", ":into(my %h) hash still leaks out";
}
