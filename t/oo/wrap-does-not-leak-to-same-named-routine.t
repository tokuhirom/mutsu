use Test;

# A wrap of `Pkg::f` runs only for the routine it wraps, not for an unrelated
# routine of the same bare name called unqualified or from another package
# (#11659).

plan 8;

module O { our sub plain() { "p" } }
&O::plain.wrap(-> | { "wp:" ~ callsame });
our sub plain() { "global" }
is plain(), 'global', 'same-named GLOBAL sub is not wrapped';
is O::plain(), 'wp:p', 'the wrapped package sub still runs its wrapper';
is GLOBAL::plain(), 'global', 'GLOBAL:: call reaches the unwrapped sub';
{
    my sub plain() { "lex" }
    is plain(), 'lex', 'lexical sub of the same name is not wrapped';
}

module P { our sub g() { "pg" } }
module Q { our sub g() { "qg" } }
&P::g.wrap(-> | { "w:" ~ callsame });
is P::g(), 'w:pg', 'wrapped package routine';
is Q::g(), 'qg', 'same-named routine of another package is not wrapped';

{ my sub f() { "a" }; &f.wrap(-> | { "w:" ~ callsame }); is f(), 'w:a', 'wrapped lexical sub'; }
{ my sub f() { "b" }; is f(), 'b', 'same-named lexical sub in another scope is not wrapped'; }
