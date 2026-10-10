# IP::Addr test suite passes

Three interpreter gaps found by the IP::Addr suite were fixed: enum values exported by a `unit module` whose declared name differs from its file now import as enum values; `require Foo::Bar` inside `sub term:<Foo::Bar>` treats its operand as a module name; and `$obj.attr = v` on a class with `has $.d handles *` assigns through the delegate's rw accessor.
