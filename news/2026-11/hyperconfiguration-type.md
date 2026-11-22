# `HyperConfiguration` is a type name

`HyperConfiguration` is the core class that `HyperSeq.configuration` answers.
In mutsu it existed only as the instance class the runtime builds, so the bare
name was a compile-time "Undeclared name" error (#18709).

It now has a builtin-catalog row (`.^mro` is `HyperConfiguration, Any, Mu`), so
the name resolves and smartmatching works. A `new` row takes `:batch` and
`:degree`, and `.gist` renders like `.raku`
(`HyperConfiguration.new(batch => 2, degree => 3)`).
