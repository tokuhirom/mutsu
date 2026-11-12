# A `require`d unit no longer resolves names through its loader's imports

`require ::($module)` from inside a routine of another module ran the required
unit's mainline with the loader's routine frame still counted as "the running
package". Type names in the required unit were then resolved through the
aliases the *loader's* package had imported. EEL's CLI (in package `EEL::CLI`,
which sees EEL::Slang's exported `role Grammar[...]`) requires the user's
transform module, and that module's `grammar LogLine` inherited the parametric
role instead of the core `Grammar` and died with "No matching candidate found
for the parametric role".

The running-package walk now stops at the routine depth the module mainline
started at, so a loaded unit only sees its own packages, as in Rakudo, where a
required compunit's outer scope is the setting.
