# Operators imported by a bare module file stay visible to its routines

A module file with no `unit` declarator that imports a custom operator
(`use Provider;` where `Provider` exports `infix:<¢¿>`) lost the operator as soon
as its load returned: its exported subs failed with "Two terms in a row" when
called later, because the import alias lived in `GLOBAL::` and was dropped when
the loading scope's entries were restored. Operator aliases are now secluded to
the importing compunit like plain routine aliases.

A bare file loaded from inside a `unit module` also had its operator imports
installed into the enclosing unit module's package instead of its own, so even
its load-time constants could not see them. The enclosing module is now used only
when the body running actually declares a unit module.

Found by working MUGS::UI::TUI: `use MUGS::UI::TUI` died in
`Terminal::Widgets::ColorThemes` and now loads, as under rakudo.
