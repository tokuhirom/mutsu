use v6;
use Test;

# A statement label on a C-style `loop (init; cond; step) { ... }` must parse.
# Regression: mutsu's labeled-loop parser only handled the infinite `loop { }`
# form after a label, so `LABEL: loop (my $i = 0; $i < 3; $i++) { ... }` died
# with "Confused. expected statement". Labels on `for`/`while`/infinite-`loop`
# already worked; the C-style header was the gap. (Hit by IDNA::Punycode.)

plan 6;

# Basic labeled C-style loop runs to completion.
{
    my @seen;
    LOOP1:
    loop (my $i = 0; $i < 3; $i++) {
        @seen.push($i);
    }
    is @seen.join(','), '0,1,2', 'labeled C-style loop iterates';
}

# `last LABEL` from a labeled C-style loop exits it.
{
    my @seen;
    LOOP2:
    loop (my $i = 0; $i < 10; $i++) {
        last LOOP2 if $i == 2;
        @seen.push($i);
    }
    is @seen.join(','), '0,1', 'last LABEL exits a labeled C-style loop';
}

# `next LABEL` continues the labeled C-style loop.
{
    my @seen;
    LOOP3:
    loop (my $i = 0; $i < 4; $i++) {
        next LOOP3 if $i == 1;
        @seen.push($i);
    }
    is @seen.join(','), '0,2,3', 'next LABEL continues a labeled C-style loop';
}

# `last OUTERLOOP` from an inner labeled C-style loop breaks the outer loop.
{
    my @seen;
    OUTERLOOP:
    for 1..3 -> $x {
        INNERLOOP:
        loop (my $k = 0; $k < 3; $k++) {
            last OUTERLOOP if $x == 2 && $k == 1;
            @seen.push("$x-$k");
        }
    }
    is @seen.join(','), '1-0,1-1,1-2,2-0', 'last OUTERLOOP from inner C-style loop';
}

# Label on the same line as the C-style loop also parses.
{
    my @seen;
    SAME: loop (my $i = 0; $i < 2; $i++) { @seen.push($i); }
    is @seen.join(','), '0,1', 'same-line label on C-style loop';
}

# The infinite `loop { }` label form still works (no regression).
{
    my @seen;
    my $i = 0;
    INF:
    loop {
        last INF if $i == 3;
        @seen.push($i);
        $i++;
    }
    is @seen.join(','), '0,1,2', 'labeled infinite loop still works';
}
