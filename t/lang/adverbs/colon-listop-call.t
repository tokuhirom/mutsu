use Test;

plan 12;

# A statement-initial `IDENT:` followed by whitespace is a statement label,
# even when `IDENT` names a declared subroutine: rakudo 2026.11 (both
# frontends) reads `trim: "hi"` as the label `trim` on the statement `"hi"`,
# not as a colon-listop call (#18283).
sub trim($x) { $x ~ "!" }
sub wrap1() { trim: "hi" }
is wrap1(), "hi", 'IDENT: at statement start labels the statement, even for a sub name';

sub add2($a, $b) { $a + $b }
sub wrap2() { add2: 3, 4 }
is wrap2(), '3 4', '... the labelled statement is the whole comma list';

sub upper($s) { $s.uc }
sub wrap3() { upper: "abc".substr(0, 2) }
is wrap3(), "ab", '... and a method chain is not an argument';

my @log;
sub record($x) { @log.push($x) }
sub run-it() { record: 99 }
run-it();
is-deeply @log, [], '... so the sub is not called';

# The sub is still callable the usual ways.
is trim("x"), "x!", 'a parenthesized call';
is (trim "y"), "y!", 'a listop call';

# Labels on loops still work.
my @seen;
OUTERLOOP: for 1..2 -> $i {
    for 1..2 -> $j {
        @seen.push("$i$j");
        next OUTERLOOP if $j == 1;
    }
}
is-deeply @seen, ['11', '21'], 'loop label still works (next LABEL)';

# A label on a bare block still runs the block.
my $ran = 0;
NOPE: { $ran = 42; }
is $ran, 42, 'label on a bare block still runs';

# A labelled statement's value is its statement's.
sub lab1() { FOO: 42 }
is lab1(), 42, 'a labelled tail statement is the routine value';
is (do { FOO: 42 }), 42, '... the do-block value';
is (BAR: 43), 43, '... a parenthesized statement value';
is EVAL(q[BAZ: 44]), 44, '... the EVAL value';
