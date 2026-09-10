use Test;

plan 7;

# `foo: args` at statement position, where `foo` is a declared subroutine,
# is a colon-listop call (Raku), not a label. This is the form used as the
# sole statement of a routine body, so it is observable via the return value.
sub trim($x) { $x ~ "!" }
sub wrap1() { trim: "hi" }
is wrap1(), "hi!", 'colon-listop call as sub body statement';

sub add2($a, $b) { $a + $b }
sub wrap2() { add2: 3, 4 }
is wrap2(), 7, 'colon-listop call with multiple args';

# A chained method-call argument after the colon.
sub upper($s) { $s.uc }
sub wrap3() { upper: "abc".substr(0, 2) }
is wrap3(), "AB", 'colon-listop with a method-chain argument';

# The side effect of a colon-listop call statement runs.
my @log;
sub record($x) { @log.push($x) }
sub run-it() { record: 99 }
run-it();
is-deeply @log, [99], 'colon-listop call statement runs its effect';

# Labels on loops still work (undeclared name → label).
my @seen;
OUTER: for 1..2 -> $i {
    for 1..2 -> $j {
        @seen.push("$i$j");
        next OUTER if $j == 1;
    }
}
is-deeply @seen, ['11', '21'], 'loop label still works (next LABEL)';

# A label on a bare block still works when the name is not a sub.
my $ran = 0;
NOPE: { $ran = 42; }
is $ran, 42, 'label on a bare block still runs';

# An undeclared name with colon + statement is a label, not a call.
lives-ok { MYLABEL: my $x = 5; $x }, 'label before a my-declaration parses';
