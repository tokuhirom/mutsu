# RakuAST model fields participate in attribute introspection

RakuAST type objects and node values now expose their implemented model fields
through `.^attributes(:local)`. Each field is returned as the same `Attribute`
object used by ordinary classes, including its name, declaring package, and
unconstrained `Mu` type.

The metadata deliberately describes mutsu's public model fields rather than
Rakudo's compiler-backend storage slots. This completes RakuAST Phase 3 slice 9.
