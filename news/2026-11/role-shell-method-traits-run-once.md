# A role's method traits run once, not again from its early-registered shell

mutsu registers a type declared after mainline code early, as a "shell" at
the head of its scope (after the unit's leading `use`s since #18103), so the
code before it can already reach the type. A class shell drops its methods'
user traits, but a role shell kept its whole body, traits included: a
`method m is also<x>` trait in a role ran once from the shell and again at the
real declaration.

Method::Also showed why that matters. Its `is also` trait mixes a
`specialize` hook into the role's HOW, and that hook mixes a `compose` hook
into the HOW of every class consuming the role. Run from the shell, it
reached the class before the class's real declaration, which took the hook
as one the class already had and never ran it, so none of the aliases the
hook installs existed (`No such method 'bar' for invocant of type 'Bar'`).
A role shell now drops its methods' user traits the way a class shell does,
and Method::Also's suite matches rakudo again.
