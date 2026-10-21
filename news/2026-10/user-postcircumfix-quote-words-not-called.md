# A declared `postcircumfix:<< >>` no longer hijacks `$x<<key>>`

`$x<<key>>` and `$x«key»` are the `{ }` subscript of a quote-words key; Rakudo never calls a
user-declared `postcircumfix:«<< >>»` for them. mutsu parsed the subscript as a call of that
operator (and died with `Undeclared routine: foo` or returned the operator's result). The
user-postcircumfix matcher now skips the quote-words openers, so the built-in subscript runs.
This retires ADR-8751's "`postcircumfix:<< >>` keeps its imported candidates in the registry"
leftover: there is nothing to bind lexically, because no candidate is ever called.
