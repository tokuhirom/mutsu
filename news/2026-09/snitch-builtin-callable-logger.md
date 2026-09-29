# Pass built-in callable loggers to snitch

The `dd` routine now resolves as a callable value through `&dd`. This lets
`.snitch(&dd)` log its invocant and return it unchanged, just as `.snitch(&note)`
does. An unitemized Seq reaches a custom snitcher as a List, matching Rakudo.
A focused regression covers both built-in loggers and the Seq argument type.
The socket smoke test now uses a loopback listener with an assigned port, so
its result does not depend on external DNS or network access.
