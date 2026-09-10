use v6;
use Test;

# A coercion-typed parameter `Target(From) $p = <default>` must NOT raise the
# compile-time X::Parameter::Default::TypeCheck: raku defers coercion-default
# checking to runtime binding (`raku -c` reports "Syntax OK" even for a default
# that will never bind). Regression: mutsu rejected every coercion default whose
# target contained `::` (e.g. `IO::Path(Str) $p = "x"`) at parse time.
# T-022 (Devel::ExecRunnerGenerator).

plan 7;

# Positional coercion default whose value matches the `From` type.
lives-ok {
    sub f(IO::Path(Str) $p = "default.txt") { $p }
    f();
}, 'IO::Path(Str) $p = "default.txt" parses and runs';

# Named coercion default (the exact shape used by the dist).
lives-ok {
    sub f(IO::Path(Str) :$out-path = ".") { $out-path }
    f();
}, 'IO::Path(Str) :$out-path = "." parses and runs';

# From type is Cool; an Int default conforms to Cool.
lives-ok {
    sub f(IO::Path(Cool) $p = 42) { $p }
    f();
}, 'IO::Path(Cool) $p = 42 parses and runs';

# Target itself matches the default directly.
{
    sub f(Int(Str) $p = 42) { $p }
    is f().^name, 'Int', 'Int(Str) $p = 42 coerces target to Int';
}

# Version coercion.
lives-ok {
    sub f(Version(Str) $p = "1.2") { $p }
    f();
}, 'Version(Str) $p = "1.2" parses and runs';

# A plain (non-coercion) bad default is STILL rejected at compile time.
dies-ok {
    EVAL 'sub f(Int $p = "x") { }';
}, 'plain Int $p = "x" is still a compile-time error';

# A plain (non-coercion) `::`-qualified type with a mismatched default is
# still rejected -- the coercion carve-out must not leak to plain types.
dies-ok {
    EVAL 'sub f(X::AdHoc $p = "x") { }';
}, 'plain X::AdHoc $p = "x" (non-coercion) is still rejected';
