unit module AmpScope::Helpers;

# A helper sub that a role in another compunit delegates to by `&name`.
sub tab-up(Int $n = 1) is export { "sub:$n" }
