use Test;

plan 8;

multi classify-inf-nan(Numeric $) { 'numeric' }
multi classify-inf-nan(Inf) { 'inf' }
multi classify-inf-nan(NaN) { 'nan' }

is classify-inf-nan(Inf), 'inf', 'Inf dispatch beats Numeric';
is classify-inf-nan(NaN), 'nan', 'NaN dispatch beats Numeric';

{
    multi same-type($x, $y where { $x.WHAT.gist eq $y.WHAT.gist }) { 0 }
    multi same-type($x, $y) { -1 }

    is same-type(42, 42), 0, 'where-clause dispatch sees earlier positional params';
}

{
    proto numeric-main($) {*}
    multi numeric-main(Complex() $x) { $x }
    multi numeric-main(Num() $x) { $x }
    multi numeric-main(Rat() $x) { $x }
    multi numeric-main(Int() $x) { $x }

    # A coercion type `T()` is `T(Any)`: it accepts anything, so four of them
    # are equally wide for a Str argument and raku reports an ambiguity rather
    # than guessing which target type the string "looks like".
    throws-like { numeric-main('42') }, X::Multi::Ambiguous,
        'all-coercive candidates are ambiguous for a Str argument (Int-like)';
    throws-like { numeric-main('123e3') }, X::Multi::Ambiguous,
        'all-coercive candidates are ambiguous for a Str argument (Num-like)';
}

{
    role Foo[::] { }
    class Bar {
        proto method baz($ --> Bool:D) {*}
        multi method baz(::T: $ where Foo[T] --> True) { }
        multi method baz($ --> False) { }
    }

    is-deeply Bar.baz(Foo[Bar]), True, 'anonymous role subset beats fallback candidate';
}

{
    multi outer-shadow(Int $value) { "outer $value" }

    {
        my sub outer-shadow(::T \value --> T) { value }

        is-deeply outer-shadow(42), 42, 'lexical plain sub shadows outer multi family';
    }

    throws-like {
        my sub bad-shadow(::T \value --> T) { value.Num }
        bad-shadow(42)
    },
        X::TypeCheck::Return,
        got => 42e0,
        'return type checks preserve captured types and got payload';
}
