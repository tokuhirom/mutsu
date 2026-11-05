# A compound-named class's own short name does not shadow an imported type inside it

Inside `class X::Errno { ... Errno($code) ... }` the call named the class itself
(`X::Errno.new($code)`, endless recursion) instead of the enum `Errno` the unit `use`d:
the class registers its short name in its prefix package `X`, and both bare-name resolvers
found that entry before the unit's import. Both now skip the class's own entry and fall
through to the outer scope, as rakudo does. Universal::Errno now loads and runs.
