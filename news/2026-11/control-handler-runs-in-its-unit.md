# A module's `CONTROL` handler resolves names in its own module

mutsu runs a `CONTROL` handler inline at the raise site, so it entered the
installing frame's env and slots but kept the raise site's compunit and
package. A module's handler then could not call the module's own file-scope
or `our` routine when the control exception came from code in another
compunit. The handler entry now records the unit and package it was installed
in, and the inline run switches to them. Red's `row-predicate` catches
`CX::Red::Bool` from a policy method declared in the test file and calls its
file-private `row-expression-key`; every protected query in Red 0.2.17's
row-security tests died with `Unknown function: row-expression-key` (#19176).
