# CompUnit::Loader, Deprecation and Rakudo::Internals class methods are rows

The method-call entry no longer matches four class methods by name. These are now class-method rows of the one
method table (ADR-11276 §9.159):

- `Rakudo::Internals.REGISTER-DYNAMIC`
- `Deprecation.report`
- `CompUnit::Loader.load-source`
- `CompUnit::Loader.load-source-file`

`.can` finds them as it does Rakudo's.
