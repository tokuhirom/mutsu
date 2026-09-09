# Hash mixins preserve their contents during assignment and mutation

`%(2 => 3) but Role` now remains an associative Hash value when it is bound,
assigned to a `%` variable, indexed, or deleted from. Existing entries are no
longer lost when a later store falls through the ordinary Hash assignment path.

Parameterized `Associative[Value, Key]` mixins also report their value type
through `.of`, while retaining normal Hash mutation behavior.
