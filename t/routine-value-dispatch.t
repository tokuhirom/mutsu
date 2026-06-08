use Test;

# Regression pin for ③ PR-1: Routine value dispatch (vm_dispatch_helpers
# vm_call_on_value) now routes user subs/multi/proto through the VM's unified
# compiled-first entry instead of the raw interpreter.call_function fallback,
# while preserving builtin priority for builtin-named Routines (e.g. &SETTING::not).

plan 10;

# &?ROUTINE recursion produces a Routine value that resolves to the user sub.
sub fact($n) { $n <= 1 ?? 1 !! $n * &?ROUTINE($n - 1) }
is fact(5), 120, '&?ROUTINE recursion via Routine value (compiled)';
is fact(1), 1, '&?ROUTINE base case';

# A Routine value stored in a scalar and invoked.
sub greet($x) { "hi $x" }
my $r = &greet;
is $r("bob"), "hi bob", 'Routine value stored and called';

# Routine value over a multi sub.
multi describe(Int $x) { "int:$x" }
multi describe(Str $x) { "str:$x" }
my $d = &describe;
is $d(7), "int:7", 'Routine value resolves multi (Int)';
is $d("z"), "str:z", 'Routine value resolves multi (Str)';

# Builtin priority preserved: &SETTING::...::not refers to the builtin even
# when a user sub shadows the name.
sub not($x) { "USER" } #OK shadow
is &SETTING::not(False), True, '&SETTING::not uses builtin (priority preserved)';
is &SETTING::not(True), False, '&SETTING::not builtin negates True';

# Routine value invoked via map (block dispatch path).
sub dbl($n) { $n * 2 }
my @out = (1, 2, 3).map(&dbl);
is @out, [2, 4, 6], 'Routine value as &dbl through map';

# Junction of callables threads the dispatch.
sub inc($n) { $n + 1 }
sub dec($n) { $n - 1 }
my $j = &inc | &dec;
ok $j(10) == 11 | $j(10) == 9, 'Junction of Routine values threads dispatch';

# Package-qualified user sub via Routine.
package Calc { our sub sq($n) is export { $n * $n } }
my $sq = &Calc::sq;
is $sq(4), 16, 'package-qualified Routine value (compiled)';
