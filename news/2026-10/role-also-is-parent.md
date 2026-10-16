# `also is Parent;` in a role body

`role R { also is Base; ... }` and `unit role R; also is Base;` died ("Undeclared
routine: also" / "Two terms in a row" at instantiation). The statement is now the
header's `role R is Base`, so the role (and a class composing it) inherits `Base`.
Gnome::Gio::EmblemedIcon is written this way; its remaining failures are the
native-sub-in-role gap (#13645) and the `Variant` crash (#13145).
