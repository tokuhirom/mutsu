use v6;
use Test;

# A coercion type constraint (`Type()`) on a parameter/variable is a ONE-SHOT
# conversion applied where the value is bound or assigned, not a persistent
# declared type. A later `:=` (bind) of that same name must not re-check or
# re-coerce through it: binding replaces the whole container, verbatim, the
# way it does for any other name. A genuine DECLARED type (no parens) is the
# opposite: `:=` still type-checks against it (a real constraint), it just
# never coerces.
#
# Found via Email::MIME's `Email::Simple.create(Array() :$header is copy, ...)`
# (from the vendored Email::Simple distribution): `$header := $header-class.new($header, ...)`
# re-coerced the freshly-built header OBJECT back through the stale `Array()`
# constraint, leaving `$header` an Array and breaking every method call on it
# ("No such method 'header' for invocant of type 'Array'").

plan 5;

sub coerce-rebind(Array() :$header is copy) {
    my $before = $header.^name;
    $header := "rebound";
    return ($before, $header.^name, $header);
}
my ($before, $after, $val) = coerce-rebind(header => []);
is $before, 'Array', 'the coercion applied at parameter binding, as normal';
is $after, 'Str', ':= replaces the container outright, no re-coercion';
is $val, 'rebound', 'the bound value itself is untouched';

# Plain assignment (`=`), unlike `:=`, keeps re-applying the coercion on every
# store -- only binding is exempt.
sub coerce-assign(Array() :$header is copy) {
    $header = "reassigned";
    return $header.^name;
}
is coerce-assign(header => []), 'Array', '= (unlike :=) keeps coercing on every store';

# A genuine DECLARED type (no coercion parens) is the opposite of both: `:=`
# still type-checks against it, so a mismatched bind dies rather than silently
# rebinding.
{
    my Array $x = [];
    dies-ok { $x := "not an array" }, ':= to a declared (non-coercion) typed var still type-checks';
}
