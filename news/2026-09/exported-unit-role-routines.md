# Exported routines from unit roles

Exported routines declared in a `unit role` are now importable as soon as the
role is loaded.  Previously mutsu deferred their registration until a class
composed the role, leaving imports such as Red's `create-resultseq` unavailable
during custom metamodel composition.
