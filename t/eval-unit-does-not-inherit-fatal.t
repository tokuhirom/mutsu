use Test;
use MONKEY-SEE-NO-EVAL;

# `fatal` is lexical to a compilation unit, and EVAL compiles a fresh one, so an
# EVAL'd snippet does NOT inherit the caller's `fatal` -- neither an explicit
# `use fatal` nor the implicit one a `try` block turns on. Only a snippet that
# says `use fatal` itself is fatal.
#
# Under `fatal`, assigning an unhandled Failure to a variable throws; without it
# the Failure just sits in the variable. That difference is what makes this
# observable.
#
# Verified assertion-for-assertion against rakudo.

plan 14;

# --- the caller's fatal does not reach the snippet ------------------------
is (try EVAL 'my $x = Failure.new; 1'), 1,
   'a plain EVAL does not fatalize an assigned Failure';
is (try EVAL 'sub f { fail "z" }; my $r = f(); 1'), 1,
   'nor one that came from fail()';
is (try EVAL 'my $x = Failure.new; 42'), 42,
   'the snippet returns its own last value';
is (try EVAL '{ my $x = Failure.new; }; 1'), 1,
   'the same inside a nested block in the snippet';
is (try EVAL 'my $z is default(Failure.new); 1'), 1,
   'a Failure used as an `is default` value does not throw';

{
    use fatal;
    is (try EVAL 'my $x = Failure.new; 1'), 1,
       'an enclosing `use fatal` does not reach the snippet either';
    is (try EVAL 'sub f { fail "z" }; my $r = f(); 1'), 1,
       'nor for a fail()-produced Failure';
}

# --- a snippet that turns fatal on itself IS fatal ------------------------
{
    my $ok = (try EVAL 'use fatal; my $x = Failure.new; 1').defined;
    nok $ok, 'a snippet with its own `use fatal` throws on the assignment';
}
{
    my $ok = (try EVAL 'use fatal; sub f { fail "z" }; my $r = f(); 1').defined;
    nok $ok, 'and on a fail()-produced Failure';
}

# --- an unhandled Failure as the snippet's VALUE still throws -------------
# This is not about `fatal` at all: the Failure becomes EVAL's return value and
# is then sunk by the caller.
{
    my $ok = (try EVAL 'Failure.new').defined;
    nok $ok, 'a Failure as the last statement still throws';
}
{
    my $ok = (try EVAL 'my $x = Failure.new').defined;
    nok $ok, 'so does an assignment as the last statement, whose value it is';
}

# --- the caller's own fatal is unchanged afterwards -----------------------
{
    use fatal;
    EVAL '1';
    my $threw = False;
    { my $x = Failure.new; 1; CATCH { default { $threw = True } } }
    ok $threw, 'the caller keeps its own `use fatal` after an EVAL returns';
}
{
    EVAL 'use fatal; 1';
    my $x = Failure.new;
    ok True, 'a snippet turning `use fatal` on does not leak it to the caller';
    is $x.defined, False, 'and the caller can still hold a Failure';
}

# vim: expandtab shiftwidth=4
