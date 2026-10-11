# RakuAST retains keyed attributes and composed type constraints

Hash attributes retain their written key type separately from their optional
value type. `has %.h{Str}` has a shape and no explicit value type, while
`has Any %.h{Str}` keeps the written `Any`. The compiler combines those fields
once when it builds its existing attribute descriptor, preserving runtime
key and value checks.
Multiple key dimensions retain one type expression per shape statement.

Definedness, coercion and parameterization compose recursively in both RakuAST
directions, with a 128-level limit. Type names use the parser's identifier
grammar, including hyphens, apostrophes and Unicode. Typed C-style loop setup
uses ordinary declaration conversion, retaining its initializer and type.
The VM's declaration check also resolves the bare target of a definite
coercion such as `Int:D(Str)` before checking whether that type is declared.

The focused cohort checks measured source fields, round-trip execution and
constructed type trees against Rakudo. Attribute conversion and type building
now live in separate modules, and the precompilation schema is version 25.
Hand-built variable declarations retain their scope, type, shape and constraints;
attribute twigils use that same constructor and lower through the shared compiler.
Named, accessor and computed object-hash stores share key validation and coercion
before mutation, so a rejected key cannot enter a typed attribute's container.
