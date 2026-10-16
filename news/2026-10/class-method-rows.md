# Built-in classes' class methods are method-table rows

`Supply.from-list` and `.on-demand`, `CompUnit::RepositoryRegistry.repository-for-name`, `.repository-for-spec` and
`.run-script`, `Encoding::Registry.find` and `.register`, and the sockets' `connect`, `listen`, `bind-udp` and `udp`
are now rows of the classes Rakudo declares them on (ADR-11276 §9.122), in place of name-matching arms that each asked
which type object the receiver was. `.^can` sees them, and `from-list`/`on-demand` called on a `Supply` instance die
with Rakudo's "must be a type object" message.
