# VERS: `if ... -> @x` Seq binding, versioned-class BEGIN, Version subclass identity

Working the VERS distribution (`t/01-basic.rakutest`, 0/36 -> 36/36 under mutsu) exposed five
general gaps, each pinned by a `t/` test:

- `if EXPR -> @x` now caches a `Seq` condition into a List like an `@` parameter does, instead of
  dying with "expected Positional but got Seq" (`t/control/if-pointy-array-param-seq.t`).
- A `class C:ver<..>:auth<..>` declaration (wrapped with its `__MUTSU_SET_META__` call) is now hoisted
  into the BEGIN prologue like an unversioned class, so `BEGIN { C.^add_method ... }` sees it
  (`t/oo/begin-add-method-versioned-class.t`).
- `^add_method` of an operator code value (`&[==]`) forwards through the code value, so a user
  `multi infix:<==>` declared elsewhere no longer hijacks its signature
  (`t/oo/add-method-operator-with-user-multi.t`).
- `Version.new` treats any non-alphanumeric character (`!`, ...) as a separator, as rakudo does.
- A `Version` subclass instance is a value type: `===`, `.repeated` and `.unique` compare by value
  (`t/types/version-subclass-identity.t`).
