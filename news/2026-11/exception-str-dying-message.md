# `Exception.Str` of a user class whose `message` dies falls back to the placeholder

A user exception class whose `method message` dies made `.Str`, `.gist` and
string interpolation of an instance die with the inner error. Rakudo swallows
it: `Str` is `Something went wrong in (C)` (the class's last name part) and
`gist` renders as for an exception with no message. mutsu now does the same;
calling `.message` directly still dies (#16066).
