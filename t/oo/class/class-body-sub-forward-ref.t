use v6;
use Test;

# A `sub` declared inside a class body is hoisted to the top of the class
# body's own scope at compile time, exactly like a top-level `sub` is hoisted
# to the top of the compilation unit (AGENTS.md's `hoist_sub_decls`) -- so a
# class-body statement written *above* the `sub`'s textual position can still
# call it. Regression for the `Date::Calendar::Hijri` zef distribution, whose
# module computes two closures from a helper declared later in the same class
# body:
#
#   class Date::Calendar::Hijri {
#     my ($f0, $g0) = make-fct(1, 1, -1);   # calls make-fct before its decl
#     ...
#     sub make-fct(int $a, int $b, int $c) { ... }
#   }
#
# which mutsu previously rejected with "Unknown function: make-fct".

plan 3;

class ForwardRefHelper {
    my ($sum, $diff) = combine(3, 5);

    sub combine($a, $b) {
        return $a + $b, $a - $b;
    }

    method sum { $sum }
    method diff { $diff }
}
is ForwardRefHelper.new.sum, 8, 'class-body statement above a sub can call it (sum)';
is ForwardRefHelper.new.diff, -2, 'class-body statement above a sub can call it (diff)';

# The sub's own in-sequence declaration must still be the one later BEGIN-time
# code sees as "reached" -- a BEGIN block after the textual declaration must
# not be broken by the early hoist registration.
class ForwardRefBegin {
    my $early = greet();

    sub greet { "hi" }

    BEGIN {
        # Reaches this point only if hoisting didn't disturb ordinary
        # compile-time execution around the declaration.
    }

    method early { $early }
}
is ForwardRefBegin.new.early, "hi", 'BEGIN block after the sub does not disturb the hoisted call';
