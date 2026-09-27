# A role with a compound name can name itself inside its package

A role declared with a compound name inside a package — `module Core { role
A::Item { ... } }`, or Intl::CLDR's `unit module Core; role CLDR::Item` —
registers as `Core::A::Item`, and its own method signatures name it by the
relative spelling `A::Item` (`multi method AT-KEY(CLDR::Item:U: $k)`). Two
gaps stopped that:

- role-method registration accepted only the full registered name or its last
  segment as a self-reference, so `A::Item:U` died at declaration with
  "Invalid typename 'A::Item:U' in parameter declaration." — the error every
  `Intl::CLDR::Types::*` module failed to load with;
- the role declaration never bound the as-written compound name, so once the
  signature was accepted a parameter typed `A::Item` still rejected an
  instance of a composing class ("expected A::Item but got Any"). The class
  path already registered that alias; the role path now does the same.

Pinned by `t/oo/role/role-method-param-names-own-relative-qualified-name.t`.
