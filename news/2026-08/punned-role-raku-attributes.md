# Punned role Raku attributes

Punning a role temporarily materializes a class so construction can use the
ordinary class path, then withdraws that shell so the type remains a role. The
generic instance renderer previously relied only on class metadata and therefore
omitted public attributes after the shell was withdrawn. It now falls back to the
role's composed attribute declarations, preserving `.raku` for both defined and
undefined values, including roles created through `EVAL`.
