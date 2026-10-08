use Test;

# From the DateTime::Timezones distribution: `DateTime.^find_method('timezone')`
# answered Mu (a built-in outside the introspected owner set) although
# `.^can` found it, so its load-time `.wrap` died.
plan 7;

for <timezone in-timezone year Str> -> $name {
    isa-ok DateTime.^find_method($name), Method, "DateTime.^find_method('$name') is a Method";
}
isa-ok Date.^find_method('day'), Method, "Date.^find_method('day') is a Method";
is DateTime.^find_method('year').name, 'year', 'name round-trips';
nok DateTime.^find_method('no-such-method').defined, 'unknown name stays undefined';
