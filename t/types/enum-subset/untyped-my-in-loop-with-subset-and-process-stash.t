use Test;

# Two process-wide facts used to push every `my` declaration of a program off
# the lean declaration lane (#9170): any `PROCESS::<...>` read anywhere (Cro's
# UnhandledErrorReporter) and any `subset` anywhere. Neither concerns a plain
# untyped `my $x`, so its answers must not change, and the typed neighbours
# must still be checked.

plan 12;

subset Even of Int where * %% 2;

PROCESS::<$PROBE-DYNAMIC> = 'from PROCESS';
sub read-probe { PROCESS::<$PROBE-DYNAMIC> }
is read-probe(), 'from PROCESS', 'a PROCESS:: dynamic is readable from a sub';
is $*PROBE-DYNAMIC, 'from PROCESS', '... and as a dynamic variable';

my @seen;
for ^3 { my $s = "x" ~ $_; @seen.push($s) }
is @seen.join(','), 'x0,x1,x2', 'an untyped my in a loop beside a subset and a PROCESS:: read';

my $acc = 0;
for ^5 { my $n = $_ * 2; $acc += $n }
is $acc, 20, 'arithmetic through an untyped my';

for ^2 { my $t = $_; $t = $t + 10; @seen.push($t) }
is @seen[3, 4].join(','), '10,11', 'an untyped my reassigned in the loop body';

my Even $e = 4;
is $e, 4, 'a subset-typed my still works';
throws-like { my Even $bad = 3 }, X::TypeCheck::Assignment, 'and still checks its constraint';

my int $i = 5;
$i = 7;
is $i, 7, 'a native int slot stores';
dies-ok { $i = 'str' }, 'and still rejects a Str';

my Int $j = 3;
throws-like { $j = 'x' }, X::TypeCheck::Assignment, 'an Int-typed my still checks';

for ^2 {
    my $k = $_;
    my $inner = -> { $k + 100 };
    @seen.push($inner());
}
is @seen[5, 6].join(','), '100,101', 'a closure over a loop-local untyped my';

sub caller-sees-nothing { my $hidden = 'caller'; callee() }
sub callee { (try EVAL q[$hidden]) // 'nv' }
is caller-sees-nothing(), 'nv', "a callee's EVAL does not see the caller's untyped my";
