use Test;

plan 15;

# A numeric sprintf directive numifies a Str argument; one that is no number
# is X::Str::Sprintf::Directives::BadType, not a silent 0.
for <d x o b e f g c> -> $d {
    throws-like { sprintf("%$d", "x") }, X::Str::Sprintf::Directives::BadType,
        directive => $d, "%$d rejects a non-numeric Str";
}
throws-like { sprintf("%i", "x") }, X::Str::Sprintf::Directives::BadType,
    directive => 'd', '%i reports the %d directive';
throws-like { "%d".sprintf("x") }, X::Str::Sprintf::Directives::BadType,
    directive => 'd', 'the method form throws too';

is sprintf("%d", "42"), '42', 'a numeric Str still formats';
is sprintf("%x", "42"), '2a', '%x of a numeric Str';
is sprintf("%d", "4.5"), '4', '%d truncates a decimal Str';
is sprintf("%s", "x"), 'x', '%s takes any Str';
is sprintf("%d", ""), '0', 'the empty Str numifies to 0';
