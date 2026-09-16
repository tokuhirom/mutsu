# Anonymous subs preserve return traits for `Function::Validation`

An anonymous `sub (...) returns T { ... }` now keeps its return constraint in
its runtime `Signature`. The parser previously discarded the `returns T` trait
after the parameter list, so code inspecting the signature saw the default
`Mu` and validators rejected otherwise valid functions.

The return trait is now carried into the existing compiler and runtime path,
which also restores return-type enforcement for anonymous subs. The
`Function::Validation` 1.0.1 distribution moves from 22/24 to 24/24 matching
assertions and from red to green.
