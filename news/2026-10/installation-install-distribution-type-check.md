# `Installation.install` / `.uninstall` check their `Distribution` argument

`CompUnit::Repository::Installation.install(Any)` and `.uninstall(Any)` succeeded silently. Both
take `Distribution $distribution`, so they now fail the parameter binding with
`X::TypeCheck::Binding::Parameter` like Rakudo, before anything touches the repository (#9887).
`t/modules/compunit/cur-install-content-api.t` passed a class that did not do `Distribution`;
it now composes the role, which Rakudo requires too.
