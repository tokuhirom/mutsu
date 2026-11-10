# EVAL($ast) finds the ContainerDescriptor prelude

The `ContainerDescriptor` prelude is gated on the unit naming `$!descriptor`. A unit run from an AST has no
source text, so its text view (`CodeText::from_unit`) now spells that name when a string literal carries it, and
`nqp::getattr(@a, Array, '$!descriptor')` plus a bare `ContainerDescriptor` resolve as they do from source
(`t/vm/nqp-container-descriptor.t` joins the EVAL($ast) ratchet, #16356).
