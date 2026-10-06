#| B's doc.
sub shared-name($x) { 1 }
sub why-in-b is export { &shared-name.WHY.Str }
