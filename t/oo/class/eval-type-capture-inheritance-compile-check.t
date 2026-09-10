use Test;

plan 3;

# Rakudo rejects `class C is T {}` where `T` is an enclosing routine/block's
# type-capture parameter (`::T`) at compile time — before the enclosing
# block/routine is ever called, and even when it is never called at all
# (X::Inheritance::Unsupported). `run.rs`'s top-level mainline pipeline
# already ran this check (`check_type_capture_inheritance`), but the general
# `EVAL` builtin's own parse-and-check pipeline
# (`Interpreter::parse_and_eval_with_operators`) did not, so an `EVAL`'d
# string with this shape silently defined the block/class instead of dying —
# only reachable in practice through the vendored real `Test.rakumod`'s
# `throws-like`, whose string form is a genuine `EVAL $code, context => ...`
# (`todo/deep/vendor-real-test-module.md`, `t/error-reporting-quality.t`
# under `MUTSU_REAL_TEST=1`).

throws-like q{ -> ::TC129906 { class :: is TC129906 {} } },
    X::Inheritance::Unsupported, message => /TC129906/,
    'EVAL of a pointy-block type-capture inheritance dies without being called';

throws-like q{ sub f(::T $x) { class C-tc is T {} } },
    X::Inheritance::Unsupported, message => /'C-tc'/,
    'EVAL of a sub-scoped type-capture inheritance dies without being called';

# Same shape, but through EVAL's `context => CALLER::` form specifically —
# the exact call the real Test.rakumod's throws-like makes.
{
    sub outer() {
        EVAL '-> ::TC129906 { class :: is TC129906 {} }', context => CALLER::;
    }
    dies-ok { outer() }, 'EVAL with an explicit context => still runs the check';
}
