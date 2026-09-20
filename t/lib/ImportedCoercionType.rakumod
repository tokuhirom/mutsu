unit class ImportedCoercionType;

# Exports a role that inherits Str, for a consumer compunit to use as a
# coercion target from inside its own routines.
# See t/modules/import-export/imported-type-name-in-coercion.t.

role Wrapped is Str is export(:MANDATORY) { }

sub wrap(Str $s --> Wrapped()) is export(:MANDATORY) { "[$s]" }
