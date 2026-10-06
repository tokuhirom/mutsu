# Cool string methods work on a user subclass of IO::Path

`class MyPath is IO::Path {}` instances answered `.uc`, `.chars` and `.starts-with`
from their default `MyPath<31>` rendering. The native dispatch entry now swaps such a
receiver for its path string when the method is `Cool`-only and not overridden by the
subclass (#12149).
