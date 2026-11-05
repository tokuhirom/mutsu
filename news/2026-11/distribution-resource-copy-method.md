# `%?RESOURCES<key>.copy($dest)` with an IO::Path destination

A `Distribution::Resource` shares the `IO::Path` rows of the method table, but
those rows only admit plain arguments; an `IO::Path` destination is an instance,
so the call fell to the by-owner fallback, which never listed a resource's
class and died "No such method 'copy' for invocant of type
'Distribution::Resource'". A resource now counts as having the `IO::Path`
native methods that take an argument, and reaches the `IO::Path` owner there,
so `.copy` (and the other argument-taking path methods) works with an
`IO::Path` argument too (#16191). The no-argument rows (`Str`, `Mu`'s
`Stringy`) stay where they were, so `~$resource` still reports the deprecation
from its `Mu` frame.
