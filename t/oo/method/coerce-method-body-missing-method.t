use Test;

# An exception thrown inside a target type's `COERCE` propagates unchanged:
# it is neither "no acceptable coercion method" (a missing method the body
# calls is not a missing `COERCE`) nor rewrapped as a parameter binding
# failure. Expected values from rakudo. Reduced from PDF::Font::Loader::CSS,
# whose `CSS::Font:D()` coercion died inside `COERCE` and surfaced as
# X::Coerce::Impossible.

plan 8;

class Boom { multi method COERCE(Str:D $s) { die "inner boom" } }
class Missing { multi method COERCE(Str:D $s) { $s.nosuchmeth } }
class Fine { has $.s; multi method COERCE(Str:D $s) { self.new(:$s) } }

sub boom(Boom:D() $x) { $x }
sub missing(Missing:D() $x) { $x }
sub fine(Fine:D() $x) { $x.s }

throws-like { boom("a") }, X::AdHoc, message => 'inner boom',
    'a die in COERCE reaches the caller of a coercing parameter';
throws-like { my Boom:D() $v = "b" }, X::AdHoc, message => 'inner boom',
    'a die in COERCE reaches the assignment to a coercing variable';
throws-like { missing("a") }, X::Method::NotFound, method => 'nosuchmeth',
    'a missing method called inside COERCE is not a missing COERCE';

my @caught;
{
    missing("c");
    CATCH { default { @caught.push: .^name } }
}
is-deeply @caught, ['X::Method::NotFound'], 'a CATCH sees the exception once';

is fine("ok"), 'ok', 'a working COERCE still coerces';
throws-like { fine(42) }, X::Coerce::Impossible,
    'a COERCE with no candidate for the value is an impossible coercion';

class NoCoerce { }
sub none(NoCoerce:D() $x) { $x }
throws-like { none("z") }, X::Coerce::Impossible,
    'a target without COERCE is an impossible coercion';

sub int-it(Int() $x) { $x }
throws-like { int-it("abc") }, X::Str::Numeric,
    'a builtin coercion failure keeps its own type';
