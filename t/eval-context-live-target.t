use v6;
use MONKEY-SEE-NO-EVAL;
use Test;

# ADR-0037 Slice 4: `EVAL ..., context => $ctx` whose `$ctx` names a routine
# still live on the dynamic call stack targets *that specific* frame, past
# any intervening routine boundary -- not just the frame that happens to
# call EVAL. Measured against raku, ADR-0037 §1.1(b):
#
#   sub thrower($code) { my $ctx = CALLER::; EVAL $code, context => $ctx; ... }
#   sub caller-is-a-routine() { my $x = thrower('return 1'); say "got: $x"; return 'car-end' }
#   say caller-is-a-routine();
#   say "still alive";
#
# raku's `return` unwinds *past* `thrower` and returns `1` from
# `caller-is-a-routine`, the frame the context names -- `thrower` never
# resumes, so its own "got: $x" / 'thrower-end' never run.

plan 5;

my @log;
sub thrower($code) {
    my $ctx = CALLER::;
    my $x = EVAL $code, context => $ctx;
    @log.push('thrower saw x=' ~ $x);
    return 'thrower-end';
}
sub caller-is-a-routine() {
    my $x = thrower('return 1');
    @log.push("got: $x");
    return 'car-end';
}
is caller-is-a-routine(), 1,
    q[a live two-deep context routine's 'return' unwinds past the EVAL caller];
is @log.elems, 0,
    'neither the EVAL caller nor caller-is-a-routine ran any code after the targeted return';

# `thrower` above is a positional-light-dispatched sub (ADR-0037 §1.3's
# dispatch matrix: a mandatory-positional-only signature). This second case
# puts a *named*-light-dispatched sub (`:$code`) in the intervening position
# instead, to pin the twin decline check added to `vm_call_light_typed.rs`.
@log = ();
sub named-thrower(:$code) {
    my $ctx = CALLER::;
    EVAL $code, context => $ctx;
    @log.push('named-thrower resumed');
    return 'named-thrower-end';
}
sub caller-of-named-thrower() {
    my $x = named-thrower(code => 'return 99');
    @log.push("got: $x");
    return 'conc-end';
}
is caller-of-named-thrower(), 99,
    'the same targeting works when the intervening frame took the named-light dispatch path';
is @log.elems, 0, 'the named-light intervening frame also declined rather than caught it';

# A three-deep chain: the context is captured one frame *below* the actual
# EVAL call site (`middle` takes `CALLER::` and hands it down), so the
# targeted routine is neither the immediate EVAL caller (`innermost`) nor
# its immediate caller (`middle`), but one further up (`outermost`).
sub innermost($code, $ctx) {
    EVAL $code, context => $ctx;
    return 'innermost-end';
}
sub middle($code) {
    my $ctx = CALLER::;
    my $r = innermost($code, $ctx);
    return "middle-end:$r";
}
sub outermost() {
    my $x = middle('return "three-deep"');
    return $x;
}
is outermost(), 'three-deep',
    'targeting reaches past two intervening routine boundaries, not just one';
