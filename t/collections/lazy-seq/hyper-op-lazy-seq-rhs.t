use v6;
use Test;

# A hyper operator (`>>op<<`, `«op»`, etc.) reads each operand's elements via
# a side-effect-free peek (`Interpreter::value_to_list`) rather than forcing
# a not-yet-touched lazy source. `.map`/`.grep` return a `Seq` whose body
# defers running its callback until something actually touches it
# (ADR-0034), and a `.map`/`.grep`-over-Array result can likewise be a
# `LazyList`. Peeking such a value before anything has touched it sees the
# empty seed -- 0 elements -- even though the SAME value answers its true
# length once touched (`.elems`, `say`, another `.map`, ...). A fresh
# `.map`/`.grep` result used directly as a hyper-op operand hit exactly that:
# the operator saw a 3-element LHS against a 0-element RHS and threw
# X::HyperOp::NonDWIM instead of actually comparing (issue #8533).

plan 6;

# The exact shape from File::Find 0.2.5's t/01-file-find.rakutest.
{
    sub compare(\a, \b) {
        [&&] a >>~~<< b.map(*.IO);
    }
    my @actual = <a b c>;
    is-deeply compare(@actual, <a b c>), True,
        'a fresh .map(*.IO) Seq as a hyper-op RHS operand is forced, not peeked (issue #8533)';
}

my @a = 1, 2, 3;
is-deeply (@a >>+<< @a.map(* + 1)), [3, 5, 7],
    '>>+<< against a fresh .map result';
is-deeply (@a >>+<< @a.grep(* > 0)), [2, 4, 6],
    '>>+<< against a fresh .grep result';

# A genuinely infinite lazy source must still raise X::HyperOp::Infinite,
# not silently see it as empty (or hang trying to force it).
throws-like { @a >>+<< (1..*) }, X::HyperOp::Infinite,
    'a genuinely infinite RHS still throws X::HyperOp::Infinite';

# Already-reified operands are unaffected.
is-deeply (@a >>+<< (10, 20, 30)), [11, 22, 33],
    'a plain eager Array RHS is unaffected';
is-deeply ((1, 2, 3).Seq >>+<< (1, 2, 3).Seq), (2, 4, 6),
    'two already-reified Seq values are unaffected';
