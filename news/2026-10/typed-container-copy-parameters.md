# Typed container copy parameters check the original argument

An `Int @values is copy` parameter now accepts a correctly typed array.
Binding checks the original argument before creating its independent,
untyped copy. The copy accepts elements of other types, as in Rakudo, while
the caller's container keeps its own type constraint and contents. Named
parameters and hash copies follow the same assignment-constraint rule.
