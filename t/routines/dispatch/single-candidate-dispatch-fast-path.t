use Test;

# Guards the single-candidate fast path in push_method_dispatch_frame: when a
# method has zero or one candidate there is no other candidate to defer to, so
# the dispatch frame is skipped (and the per-call body-fingerprint work avoided).
# These cases must keep behaving exactly as before that short-circuit.

plan 7;

# 1. A plain single method still dispatches and returns its value.
class P1 {
    has $.x;
    method twice { $!x * 2 }
}
is P1.new(x => 21).twice, 42, 'single-candidate method returns correctly';

# 2. samewith re-dispatches the same single method with new args.
class Fact {
    method f($n) { $n <= 1 ?? 1 !! $n * samewith($n - 1) }
}
is Fact.f(5), 120, 'samewith works with a single candidate';

# 3. nextsame across an inheritance chain (multiple candidates) still chains.
class Base {
    method greet { 'base' }
}
class Derived is Base {
    method greet { 'derived+' ~ callsame() }
}
is Derived.greet, 'derived+base', 'callsame chains to parent candidate';

# 4. nextsame with a parent candidate forwards control.
class A2 {
    method m { 'A' }
}
class B2 is A2 {
    method m { nextsame }
}
is B2.new.m, 'A', 'nextsame defers to the inherited candidate';

# 5. A single method calling callsame with no further candidate returns Nil.
class Lonely {
    method only { callsame() }
}
nok Lonely.only.defined, 'callsame with no next candidate yields an undefined value';

# 6. Accessor methods (single candidate) keep working in a tight loop.
class Pt { has $.v }
my $sum = 0;
$sum += Pt.new(v => $_).v for ^100;
is $sum, 4950, 'single-candidate accessor dispatch in a loop';

# 7. multi-candidate method dispatch by arity still selects correctly.
class Multi {
    multi method g(Int $n) { "int:$n" }
    multi method g(Str $s) { "str:$s" }
}
is Multi.new.g(7) ~ '|' ~ Multi.new.g('hi'), 'int:7|str:hi',
    'multi-method dispatch unaffected';
