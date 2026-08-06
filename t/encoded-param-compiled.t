use Test;

# ADR-0019 C6e-3c: routines with an `is encoded(...)` (NativeCall string
# marshalling) parameter trait run through the compiled entry — the last
# param-trait exclusion in the OTF/plan-bytecode gate
# (`def_module_single_sig_body_ok_ignoring_state`) is lifted. The trait is
# inert for dispatch (actual encoding happens via an explicit `.encode(...)`
# call, not this binder), so behavior must match the interpreter arm exactly.

plan 6;

sub shout(Str $s is encoded('utf8')) { $s.uc }
is shout('hi'), 'HI', 'basic call through the module-single OTF path';
is shout('hi') ~ shout('yo'), 'HIYO', 'repeated calls reuse the compiled body';

sub countdown(Int $n is encoded('utf8')) {
    $n <= 0 ?? 'done' !! countdown($n - 1)
}
is countdown(3), 'done', 'recursive sub with an encoded param compiles and runs';

multi sub greet(Str $s is encoded('utf8')) { "str:$s" }
multi sub greet(Int $n) { "int:$n" }
is greet('a'), 'str:a', 'multi candidate with an encoded param';
is greet(5), 'int:5', 'sibling multi candidate without the trait still dispatches';

# EVAL-boundary call: the compiled routing must hold across a re-entrant
# compile too.
is EVAL(q[shout('eval')]), 'EVAL', 'encoded-param callee reached through EVAL';
