unit module RequireTypesOnly;

# A module that declares only types: no `RequireTypesOnly::`-prefixed routine.
our @LOADS;
@LOADS.push(1);

role Marker is export { }
class Box is export { has $.v }
