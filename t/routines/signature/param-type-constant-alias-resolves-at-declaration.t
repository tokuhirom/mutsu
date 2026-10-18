use Test;

# A parameter type that names a `constant` type alias is the alias's target,
# resolved where the signature is declared: a caller's same-named sigilless
# variable does not take part in the bind (#13728, from Timezones::ZoneInfo).
# Checked against rakudo.
plan 11;

constant T = int64;
constant C = Str;

sub leaps(T \year) {
    sub nonneg(T \year) { year div 4 }
    nonneg(year);
}
sub typed(C $s) { $s.chars }
sub local-typed($n) { my T $sum = 5; $sum += $n; my C $s = 'x'; "$sum$s" }
multi sub m(C $s) { 'str' }
multi sub m(Int $i) { 'int' }
class K { method len(C $s) { $s.chars } }

is &leaps.signature.raku, ':(int64 \year)', 'the signature names the target';
is &typed.signature.raku, ':(Str $s)', 'a class alias too';
{
    my \T = 5;
    my \C = 7;
    is leaps(100), 25, 'a nested sub binds against the alias, not the caller\'s T';
    is typed('abc'), 3, 'a typed parameter';
    is m('x') ~ m(3), 'strint', 'a multi candidate';
    is K.len('abcd'), 4, 'a method';
    is local-typed(3), '8x', 'a typed local in a routine';
    my &pointy = -> C $s { $s.uc };
    is pointy('q'), 'Q', 'a pointy block';
    my &anon = sub (C $s) { $s.lc };
    is anon('Q'), 'q', 'an anonymous sub';
}
my $n = 42;
throws-like { typed($n) }, X::TypeCheck::Binding::Parameter,
    'a value of another type is still rejected';
my T $top = 2;
is (try { $top = 'no'; 'stored' }) // 'rejected', 'rejected', 'a typed variable checks against the target';
