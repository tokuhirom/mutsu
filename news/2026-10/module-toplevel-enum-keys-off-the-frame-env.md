# A module's top-level enum keys no longer fill the importer's env

A module's mainline runs in the env of the frame that loads it, so the bare
keys of every enum it declared stayed behind in each frame env of the
importing program — 44 of them after `use Cro::HTTP2::RequestParser`, the
largest group of non-lexical entries every copy-on-write deep copy of a frame
env still had to copy.

Those keys now live in a per-package table off the env. A package-less module
file's enum keys are `GLOBAL` symbols and stay visible everywhere, as in
rakudo; a key declared inside a package — a `unit module`, a `module` block,
a class body — is visible to that package's own code and closures, and no
longer to the importer. That also fixes a leak: a class body's `my enum`
keys used to be reachable from the program that loaded the module.

A `Promise(supply { whenever … })` loop under that `use` deep-copies 26%
fewer env entries per iteration (5,085 → 3,765). This is the fourth slice of
ADR-0084 (#7817).
