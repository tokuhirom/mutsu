use v6;
use Test;

# A class declared inside a module shadows a built-in namespace of the same
# name when resolving an inheritance parent. Regression pin for the
# Node::Ethereum::RLP dist, whose Exception.rakumod does:
#
#   module Node::Ethereum::RLP::Exception {
#       class X is Exception { ... }
#       class X::Decode is X {};
#       class X::Decode::Length is X::Decode {};
#   }
#
# `X` collides with the built-in `X::` exception namespace, so `is X` used to
# resolve to the built-in namespace (an unknown inheritance parent that fell
# back to `Any`) instead of the module-local `X`, throwing
# "'X::Decode' cannot inherit from 'X' because it is unknown."

lives-ok {
    module M {
        class X is Exception {}
        class X::Decode is X {}
    }
}, 'class X::Decode is X resolves the module-local X inside a module';

lives-ok {
    module M2 {
        class X is Exception {}
        class X::Decode is X {}
        class X::Decode::Length is X::Decode {}
    }
}, 'a deeper X::Decode::Length is X::Decode chain also resolves';

# The inheritance chain is wired to the module-local X (not the built-in `X`,
# whose MRO is just `X Any Mu`). Assert on the MRO name list, which does not
# depend on how a bare `X` term resolves.
module M3 {
    class X is Exception {
        has Str $.payload is default('boom');
        method message { "X: $!payload" }
    }
    class X::Decode is X {}
    class X::Decode::Length is X::Decode {}

    my @decode-mro = X::Decode.^mro.map(*.^name);
    ok @decode-mro.grep(* eq 'Exception'),
        'X::Decode MRO reaches Exception (parent linked, not Any)';
    ok @decode-mro.grep(*.ends-with('::X')),
        'X::Decode inherits the module-local X, not the built-in X';

    ok X::Decode::Length.^mro.map(*.^name).grep(* eq 'Exception'),
        'X::Decode::Length MRO reaches Exception through the chain';
}

# A non-colliding sibling name (Y) was never affected — keep it as a control.
module M4 {
    class Y is Exception {}
    class Y::Decode is Y {}
    ok Y::Decode.^mro.map(*.^name).grep(* eq 'Exception'),
        'a non-colliding sibling name still links (control)';
}

# Top-level (no enclosing module) was already correct.
lives-ok {
    class TX is Exception {}
    class TX::Decode is TX {}
}, 'top-level class-name-as-namespace inheritance still works';

done-testing;
