# A class body's `my &code` survives the BEGIN-prologue split

When a class declares a nested class inside a method, the BEGIN prologue splits the
outer body into a declaration part and a run-time part (`Stmt::PackageRuntimeBody`).
The run-time part did not know the body's `my &code` variables: `&k = sub { ... }`
compiled to a read-only routine-name assignment ("Cannot modify an immutable value"),
and the class's methods could not call `k` afterwards. The split now carries the code
variables in the body's lexical list and the run-time compile treats them as assignable
containers. `Timezones::ZoneInfo`, `Timezone::Simple` and `DateTime::Timezones` load
again (cluster a1d6dc19); six of Timezones::ZoneInfo's seven test files pass, the last
one is a performance gap (#15079).
