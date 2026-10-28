# `nqp::bindattr` of `$!reified` works on an `is List` subclass instance

Hash2Class's `Hash2Class::List is List` is built with `nqp::create` and then
installs another list's storage with `nqp::bindattr(self, List, '$!reified',
...)`. mutsu never allocated the reserved backing array for a `List` subclass
made by `nqp::create`, and the bind landed in a plain attribute that `.elems`
and indexing do not read. `nqp::create` now allocates the backing array, the
bind installs the elements there, and `getattr(.., '$!reified')` answers the raw
storage view of it. Fixes #15138.
