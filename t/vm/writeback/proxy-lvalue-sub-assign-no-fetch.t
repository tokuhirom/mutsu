use Test;

# Found via the CSS::Font::Resources suite: assigning through an `is rw`
# routine that returns a Proxy runs STORE only. raku FETCHes when something
# reads the container, so a sink-context `f() = 1` must not fire FETCH.

plan 3;

my @log;
sub lv($n) is rw {
    Proxy.new(
        FETCH => sub ($) { @log.push("FETCH $n"); 1 },
        STORE => sub ($, $v) { @log.push("STORE $n $v") },
    );
}

lv("a") = 7;
is-deeply @log, ["STORE a 7"], 'sub lvalue assignment runs STORE alone';

@log = ();
my $x = (lv("b") = 8);
is-deeply @log[0], "STORE b 8", 'STORE runs first';
is $x, 1, 'reading the assignment result FETCHes';

done-testing;
