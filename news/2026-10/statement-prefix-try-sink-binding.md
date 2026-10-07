# `try STATEMENT if COND` and `sink A, B` bind like rakudo

A trailing statement modifier on `try STATEMENT` now binds inside the try (`try foo() if $c`, `try foo() for @x`), and `sink A, B` sinks the whole comma list rather than only the first operand. Both the `.AST` trees and the runtime behaviour (a `for` modifier now loops inside the try) match rakudo. Fixes #12240.
