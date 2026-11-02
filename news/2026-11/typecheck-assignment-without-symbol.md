# `X::TypeCheck::Assignment` without a `symbol` stringifies like Rakudo

A hand-built `X::TypeCheck::Assignment.new(:got, :expected)` has a `message`
that dies in Rakudo, so its `Str` is `Something went wrong in (Assignment)` (the
class's last name part). mutsu answered a message with an empty `$` and, once
thrown, "Died with X::TypeCheck::Assignment" for `.message` even when a
`symbol` was given. The thrown message now comes from the same builder as the
unthrown one, and `Exception.Str` falls back to the class name for a class without a message (#16047).
