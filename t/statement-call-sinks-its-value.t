use Test;

# A bare call statement discards its value, and in Raku that value is *sunk*:
# sinking an unhandled Failure throws. mutsu applied the rule only to the call
# shapes whose result lands on the stack, so a bareword statement-level call
# (`EVAL '...';`, a listop builtin) swallowed the Failure instead.

plan 8;

use MONKEY-SEE-NO-EVAL;

# The shape rakudo's own Test.rakumod uses for `throws-like '<code>'`.
throws-like { EVAL q{use fatal; "foo"[2]}; }, X::OutOfRange,
    'a sunk EVAL whose unit ends in a fatal Failure throws';

throws-like { EVAL q{use fatal; (1+2i).Real}; }, X::Numeric::Real,
    'and so does one that ends in a coercion Failure';

# Without `use fatal` the EVAL'd unit still produces a Failure, and sinking an
# unhandled Failure throws in Raku with or without the pragma.
throws-like { EVAL q{"foo"[2]}; }, X::OutOfRange,
    'the same without the pragma -- sinking an unhandled Failure always throws';

# The value-position forms were already right; they must stay right.
lives-ok { my $x := EVAL q{"foo"[2]}; $x.defined },
    'binding the Failure instead of sinking it does not throw';

lives-ok { my $x = EVAL q{1 + 1}; },
    'an ordinary EVAL value is unaffected';

# A user sub called as a bare statement is the same rule.
throws-like { sub sf { fail "boom" }; sf(); }, X::AdHoc,
    'a bare call to a user sub that fails throws';

lives-ok { sub sok { 42 }; sok(); },
    'a bare call to a user sub that succeeds is quiet';

# A handled Failure is not resurrected by the sink.
lives-ok {
    my $f = EVAL q{"foo"[2]};
    $f.defined;     # handles it
    $f;
}, 'a Failure that has been handled does not throw when sunk';
