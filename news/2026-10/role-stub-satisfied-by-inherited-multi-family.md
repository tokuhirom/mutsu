# A role stub is satisfied by an inherited multi family

`role COSType { method delegate { ... } }` composed into a class whose parent defines
`multi method delegate(::?CLASS:D:)` and `multi method delegate(::?CLASS:U:)` died with
"X::Role::Composition::Conflict: multiple inherited candidates for required method 'delegate'", so
PDF::Native could not load. rakudo only asks that the class can find a method of that name; a non-multi
stub is now satisfied by an inherited family of candidates, as it already was by a single inherited
method.
