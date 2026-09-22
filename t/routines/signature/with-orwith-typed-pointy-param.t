use v6;
use Test;

# `orwith EXPR -> TYPE $param { ... }` -- a typed pointy-block parameter on
# an `orwith` clause. `parse_elsif_chain`'s hand-rolled pointy-param scan
# only recognized a bare sigil right after `->`; a type name in front of it
# (`-> int $c`) left the type text unconsumed, made the following `block()`
# parse fail on the dangling `-> int $c { ... }` text, and that failure
# propagated all the way up to the *whole* `with`/`orwith` chain, which then
# fell back to being re-parsed as a bareword call to `orwith` ("Undeclared
# routine: orwith used") -- or, when the FIRST `with` clause also had a typed
# pointy param, as a call to `with` itself ("Unexpected block in infix
# position"), losing even the `orwith`-shaped error.
#
# This is `Identity::Utils`'s `short-name` idiom verbatim (ecosystem
# `Code::Coverage` -> `Code::Coverable` -> `Identity::Utils`):
# `with EXPR -> int $offset { ... } orwith EXPR -> int $chars { ... } else { ... }`.

sub short-name(str $identity) {
    with $identity.rindex('::') -> int $offset {
        with $identity.index(':', $offset + 2) -> int $chars {
            $identity.substr(0, $chars)
        }
        else {
            $identity
        }
    }
    orwith $identity.index(':') -> int $chars {
        $identity.substr(0, $chars)
    }
    else {
        $identity
    }
}

is short-name("Foo::Bar"), "Foo::Bar", 'no colon at all: the whole identity';
is short-name("Foo:ver<1>"), "Foo", 'a single :ver<> adverb is stripped';
is short-name("Foo::Bar:ver<1>"), "Foo::Bar",
        'a :: short name keeps its adverb stripped too';

# The minimal shape: both `with` and `orwith` typed, no nesting at all --
# this alone reproduced the parse failure without needing Identity::Utils's
# extra nesting layer.
sub classify(Int $x) {
    with $x -> int $a {
        "with $a"
    }
    orwith $x -> int $c {
        "orwith $c"
    }
    else {
        "else"
    }
}
is classify(5), "with 5", 'with/orwith both typed-pointy: the with branch fires';

# An untyped `with` whose ONLY typed clause is a later `orwith` must not
# corrupt parsing of the earlier, untyped `with` block either.
sub classify2($x, $y) {
    with $x {
        with $y -> int $b {
            "inner with $b"
        }
        else {
            "inner else"
        }
    }
    orwith $x -> int $c {
        "outer orwith $c"
    }
    else {
        "outer else"
    }
}
is classify2(1, 2), "inner with 2", 'untyped with + typed orwith parses and runs';

done-testing;
