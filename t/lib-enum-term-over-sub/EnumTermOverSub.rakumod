# Mirrors TimeUnit: an enum member and an exported sub share a name.
enum UnitName (nanos => 1, minutes => 60);
sub minutes(Numeric() $n) is export { "sub $n" }
sub unit-key(UnitName:D $u) is export { $u.key }
