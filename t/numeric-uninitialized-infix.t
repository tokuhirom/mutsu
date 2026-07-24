use v6;
use Test;

# A bare concrete-numeric type object (Int, Num, Rat, FatRat, Real, Bool) used
# as an operand of a numeric *comparison* operator throws
# X::Numeric::Uninitialized, matching Rakudo. This is a fatal exception (not a
# resumable warning, so `quietly` does not suppress it). Previously `==`/`!=`/
# `<=>` silently coerced the type object to 0 (so `Int == 0` wrongly returned
# True), and the relational ops threw a bare X::AdHoc instead of the typed error.
#
# NOTE: the arithmetic ops (`+ - * / % **`) are deliberately NOT covered here.
# `Int + 1` should also throw in rakudo, but mutsu desugars the assignment
# metaop `$a += 0.1` to the same `Add` opcode as bare infix, and that form must
# keep working (METAOP_ASSIGN identity semantics). See PLAN.md.

plan 24;

# --- Comparison operators throw ---------------------------------------------
throws-like { my $x = Int < 5 }, X::Numeric::Uninitialized, 'Int < 5 throws';
throws-like { my $x = Int <= 5 }, X::Numeric::Uninitialized, 'Int <= 5 throws';
throws-like { my $x = Int > 5 }, X::Numeric::Uninitialized, 'Int > 5 throws';
throws-like { my $x = Int >= 5 }, X::Numeric::Uninitialized, 'Int >= 5 throws';
throws-like { my $x = Int == 0 }, X::Numeric::Uninitialized, 'Int == 0 throws';
throws-like { my $x = Int != 0 }, X::Numeric::Uninitialized, 'Int != 0 throws';
throws-like { my $x = Int <=> 3 }, X::Numeric::Uninitialized, 'Int <=> 3 throws';
throws-like { my $x = 5 > Int }, X::Numeric::Uninitialized, 'type object on the right throws';

# --- Other concrete numeric types -------------------------------------------
throws-like { my $x = Num < 1 }, X::Numeric::Uninitialized, 'Num < 1 throws';
throws-like { my $x = Rat < 1 }, X::Numeric::Uninitialized, 'Rat < 1 throws';
throws-like { my $x = FatRat < 1 }, X::Numeric::Uninitialized, 'FatRat < 1 throws';
throws-like { my $x = Real < 1 }, X::Numeric::Uninitialized, 'Real < 1 throws';
throws-like { my $x = Bool < 1 }, X::Numeric::Uninitialized, 'Bool < 1 throws';

# --- Exception shape --------------------------------------------------------
{
    my $ex;
    { Int == 0; CATCH { default { $ex = $_ } } }
    isa-ok $ex, X::Numeric::Uninitialized, 'exception type is correct';
    is $ex.message, 'Use of uninitialized value of type Int in numeric context',
        'message matches rakudo';
    is $ex.type.^name, 'Int', '.type is the offending type object';
}

# --- Fatal, not a resumable warning: quietly does NOT suppress it -----------
{
    my $reached = False;
    { my $x = quietly (Int < 5); $reached = True; CATCH { default {} } }
    nok $reached, 'quietly does not turn the fatal error into a resumable warning';
}

# --- Non-concrete-numeric type objects do NOT throw (warn + coerce to 0) ----
lives-ok { my $x = quietly (Any < 5) }, 'Any < 5 does not throw';
lives-ok { my $x = quietly (Str < 5) }, 'Str < 5 does not throw';
lives-ok { my $x = quietly (Numeric < 5) }, 'Numeric < 5 does not throw';
lives-ok { my $x = quietly (Complex == 0) }, 'Complex == 0 does not throw';

# --- Generic ordering (cmp/before/after) compares type objects, no throw ----
lives-ok { my $x = Int cmp 1 }, 'generic cmp does not throw on a type object';
lives-ok { my $x = Int before 1 }, 'before does not throw on a type object';

# --- Defined numeric values still compare normally --------------------------
ok 3 < 4, 'ordinary Int comparison is unaffected';
