use lib 't/lib/ResDist/lib', 't/lib/ResInner/lib';
use Test;

# Under `use fatal`, an unhandled Failure produced by one *argument
# expression* of a plain function/method call must explode immediately at
# the call site, before the callee ever runs -- matching real Raku. This is
# the residual scope left after the list/array/hash composite-literal fix
# (see t/fatal-mode-composite-literal-failure.t): unlike a literal
# composite, a call's arguments are not assembled through
# `MakeArray`/`MakeHash` first, so `CallFunc`/`CallFuncNamed`/`CallMethod`/
# etc. each need their own check. See
# todo/tickets/fatal-mode-does-not-explode-failure-in-call-arguments.md.

plan 11;

# 1. The ticket's minimal repro: a Failure as one positional argument
#    explodes before the callee's body ever runs.
sub positional_arg_failure() {
    use fatal;
    my $ran-body = False;
    sub f($a, $b, $c) { $ran-body = True }
    f(1, "a".Int, 3);
    return $ran-body;
}
dies-ok { positional_arg_failure() },
    "a Failure in a positional call argument explodes under use fatal, before the callee runs";

# 2. Named-argument variant.
sub named_arg_failure() {
    use fatal;
    sub f($a, :$b) { "unreached" }
    f(1, b => "a".Int);
    return "unreached";
}
dies-ok { named_arg_failure() },
    "a Failure in a named call argument explodes under use fatal";

# 3. Method-call variant.
sub method_call_arg_failure() {
    use fatal;
    class C { method f($a, $b) { "unreached" } }
    C.new.f(1, "a".Int);
    return "unreached";
}
dies-ok { method_call_arg_failure() },
    "a Failure in a method-call argument explodes under use fatal";

# 4. Dynamic method-call variant (`.$name(...)`).
sub dynamic_method_call_arg_failure() {
    use fatal;
    class C { method f($a, $b) { "unreached" } }
    my $name = 'f';
    C.new."$name"(1, "a".Int);
    return "unreached";
}
dies-ok { dynamic_method_call_arg_failure() },
    "a Failure in a dynamic method-call argument explodes under use fatal";

# 5. Sanity: without `use fatal`, the same call still runs, passing the
#    Failure through as a soft value bound to the parameter.
sub positional_arg_failure_without_fatal() {
    sub f($a, $b, $c) { $b.^name }
    return f(1, "a".Int, 3);
}
is positional_arg_failure_without_fatal(), 'Failure',
    "without use fatal, a Failure call argument is passed through as a soft value";

# 6. Should NOT explode: a Failure caught and replaced by `try` before being
#    passed as the argument.
sub call_arg_handled_via_try() {
    use fatal;
    sub f($a, $b, $c) { "$a,$b,$c" }
    return f(1, ((try { "a".Int }) // 99), 3);
}
is call_arg_handled_via_try(), "1,99,3",
    "a Failure caught by try before being passed as an argument does not explode the call";

# 7. Should NOT explode: an ordinary call with no Failure at all still runs
#    normally under use fatal.
sub plain_call_under_fatal() {
    use fatal;
    sub f($a, $b, $c) { "$a,$b,$c" }
    return f(1, 2, 3);
}
is plain_call_under_fatal(), "1,2,3",
    "an ordinary call with no Failure argument runs normally under use fatal";

# 8. No double-explosion / regression check: a literal list-composite
#    argument still explodes exactly once, via the composite-literal check,
#    with the call itself never running.
sub composite_literal_call_arg_failure() {
    use fatal;
    my $ran-body = False;
    sub f($list) { $ran-body = True }
    f((1, "a".Int, 3));
    return $ran-body;
}
dies-ok { composite_literal_call_arg_failure() },
    "a Failure nested in a composite-literal call argument still explodes under use fatal";

# 9. Regression: the composite-literal case from
#    t/fatal-mode-composite-literal-failure.t keeps working unchanged.
sub list_literal_element_failure() {
    use fatal;
    my @a = (1, "a".Int, 3);
    return "unreached";
}
dies-ok { list_literal_element_failure() },
    "a Failure nested in a list-literal element still explodes under use fatal (no regression)";

# 10. `require EXPR` is exempt from this check: unlike an ordinary sub call,
#     `require` is a special form in real Raku. `require ::("ResDist")`
#     legitimately evaluates `::("ResDist")` to an unhandled "No such
#     symbol" Failure before the module is loaded -- `require`'s own
#     implementation inspects that Failure to derive the module name to
#     load (the shape `HTTP::UserAgent`-style lazy-`require` loaders use).
#     A first version of this fix exploded here too, breaking
#     `t/resources-in-required-module.t` (`require` wrapped in `try`, which
#     forces fatal_mode on for its dynamic extent).
sub require_symbol_lookup_failure_is_exempt() {
    use fatal;
    try require ::("ResDist");
    return ::('ResDist').greeting;
}
is require_symbol_lookup_failure_is_exempt(), 'hello from the ResInner resources',
    'require ::("Sym") is exempt from the call-argument fatal-mode explosion';

# 11. `defined EXPR` is also exempt (pinned by roast/S04-exceptions/fail.t
#     "use fatal respects defined"): it answers a plain Bool without
#     exploding, alongside the operator forms `//`, `||`, `&&`, `if`,
#     `unless`, `??!!`, `?`, `so`, `!`, `not` (which never reach this check
#     at all -- they compile to dedicated opcodes, not a call). A first
#     version of this fix exploded `defined it-will-fail()` too, breaking
#     that roast subtest.
sub defined_on_failure_is_exempt() {
    use fatal;
    sub it-will-fail() { fail "oops" }
    my $x = defined it-will-fail();
    return $x;
}
is defined_on_failure_is_exempt(), False,
    'defined EXPR is exempt from the call-argument fatal-mode explosion';
