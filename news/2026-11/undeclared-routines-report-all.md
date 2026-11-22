# Every undeclared routine is reported, each with its line

Rakudo's CHECK reports all of a unit's undeclared routines in one error:

```
Undeclared routines:
    foo-bar used at line 3
    nosuch used at line 2. Did you mean 'cosech'?
```

The list is sorted by name and gives each name at its last use. mutsu reported
only the first. Its compile-error renderer also stripped every " at line N"
from the message as if it were an internal parser location, so a single name
printed as `nosuch used. Did you mean 'cosech'?`, followed by a source snippet
rakudo does not show.

The mainline check now collects every unconditional undeclared routine. The
renderer prints an `Undeclared routine(s)` / `Undeclared name` report as is,
followed by a blank line, as rakudo does.
