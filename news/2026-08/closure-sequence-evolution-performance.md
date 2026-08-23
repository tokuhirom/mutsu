# Reused compiled repeat closures in evolutionary sequences

Small fixed-count `xx` expressions now inline a scalar constant count in the
enclosing bytecode, avoiding a synthetic closure call for every repeated
element. Dynamic counts retain their closure semantics, but reuse the closure's
already compiled bytecode through the normal call ABI instead of compiling its
AST at every repeat expression evaluation.

This removes repeated compiler work from the closure-generated candidate lists
used by evolutionary-search style loops while preserving per-element evaluation
and rw-argument behavior.
