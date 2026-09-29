# CompUnit::Repository::Installation enforces the Distribution parameter type

`CompUnit::Repository::Installation.install` and `.uninstall` now fail with
`X::TypeCheck::Binding::Parameter` (`expected Distribution but got Any (Any)`) when handed
something that is not a `Distribution`, matching Rakudo. Previously `uninstall(Any)` returned
silently, and junk passed to a real repository could touch it. The regression test runs against a
temporary repository prefix, never the site repo.
