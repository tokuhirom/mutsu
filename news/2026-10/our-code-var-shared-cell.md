# `our &code` shares one cell with its package-qualified name

`our &answer = { 42 }` in a module now goes through the same shared-cell declaration
(`DeclareOurScalar`) as a plain `our $x`. A write through the qualified name
(`&Pkg::answer = { 43 }`), including one made inside `start`, is now seen by the module's own
bare `&answer` (#11913). Previously the qualified binding changed while the module's alias kept
the old routine.
