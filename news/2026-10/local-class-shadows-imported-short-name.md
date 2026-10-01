# A local class shadows an imported class's short name

A package that `use`d a module and then declared its own class (or role) with the same short
name as one of the module's classes kept resolving the bare name to the imported class, because
the package-scoped short-name alias was written with `or_insert`. The local declaration now
replaces the alias. Found via Tinky::Declare (`class Workflow is Tinky::Workflow` inside
`module Tinky::Declare`), whose six test files now pass.
