# A late method walk reads a built-in class's roles

mutsu's MRO for a built-in type is its class chain only, but Rakudo also finds a method in a role that the class
composes. `Buf[uint8]`'s `unpack` comes from `Blob`, and an enum's `enums` comes from `Enumeration`.

The method table's late walk now reads the catalog's role list as well. After each class it lists the roles that class
composes, and after an enum's type it lists `Enumeration` (ADR-11276 §9.143). `Blob.unpack` and `Enumeration.enums`
are rows on those owners. The arms for `decode` and `default` were unreachable behind their existing rows, so they are
deleted.
