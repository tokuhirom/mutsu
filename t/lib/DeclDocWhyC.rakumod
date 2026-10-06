unit module DeclDocWhyC;
use DeclDocWhyB;

#| C's doc.
sub shared-name($x) { 2 }
sub why-in-c is export { &shared-name.WHY.Str ~ ' / ' ~ why-in-b() }
