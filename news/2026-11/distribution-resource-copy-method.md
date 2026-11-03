# `%?RESOURCES<key>.copy($dest)` with an IO::Path destination

A `Distribution::Resource` shares the `IO::Path` rows of the method table, but
those rows only admit plain arguments; an `IO::Path` destination is an instance,
so the call fell to the by-owner fallback, which never listed a resource's
class and died "No such method 'copy' for invocant of type
'Distribution::Resource'". A resource now counts as having the `IO::Path`
native methods and reaches the `IO::Path` owner there, so `.copy` (and the other
path methods a plain-argument call already answered) works with an `IO::Path`
argument too (#16191).
