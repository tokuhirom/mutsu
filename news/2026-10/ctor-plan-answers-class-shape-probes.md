# `.new` stops re-deriving its class's shape

Parsing a 60-row YAML document with YAMLish spent about a third of its
instructions in 244 object constructions: every `.new` re-ordered each
composed role's `BUILD` and `TWEAK` submethods (cloning the role's whole
definition to read one method entry), walked the MRO again for a QuantHash
base, and re-collected the class's `:D`/`:U` smileys and `is required`
attributes. All of that is a function of the class shape alone.

The construction now reads it from the class's cached constructor plan, and
the role-submethod ordering copies out only the submethods it was asked for.
`bench-yaml-parse-big` drops from 1,291M to 971M instructions (-24.8%,
callgrind, `--profile profiling`), on the way to #12917's goal of matching
rakudo.
