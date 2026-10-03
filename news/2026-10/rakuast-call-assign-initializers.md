# RakuAST preserves call-assignment initializers

Standalone variable declarations using `.=` now expose `RakuAST::Initializer::CallAssign` with a method call, as Rakudo does. The parser retains the declaration's source form while the shared expansion chooses its invocant for bytecode compilation; lowering a hand-built or round-tripped RakuAST declaration uses the same expansion. Typed and untyped declarations both round-trip.
