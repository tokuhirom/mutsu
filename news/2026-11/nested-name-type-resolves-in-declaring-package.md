# A nested-name type of a module resolves in the declaring package

`has Window::Clauses $.windows` and an optional `Window::Clauses $w?` parameter
inside `unit module Pk` named the module's `Pk::Window::Clauses`, but the
attribute constraint and the omitted optional's type object kept the short
spelling, so `Sel.new(:$windows)` called from another package died with
`expected Window::Clauses but got Window::Clauses`. A partially qualified type
name now resolves through the owner's enclosing packages when it is not a
registered type by itself, for attribute constraints and for the type object an
omitted optional parameter binds (#16359).
