# Slang tokens can call the core `<signature>` rule

A slang token that matches `'(' <signature> ')'` (as Slang::Mosdef's `method` declarator and
Qwiratry's `mold` do) now gets a `RakuAST::Signature` for the parameter list from the Rust parser
(ADR-15762 §2.3 shim, #13370), the same way `<block>` returns a `RakuAST::Block`. The action can
hand `$<signature>.ast` to `RakuAST::Sub.new(signature => ...)`. `<trait>` is still open.
