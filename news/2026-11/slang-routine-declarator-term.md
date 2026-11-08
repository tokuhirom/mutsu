# Slang `routine-declarator` / `scope-declarator` productions also run in term position

A slang's `routine-declarator:sym<...>` or `scope-declarator:sym<...>` token is now tried where a
term starts as well as at statement start (ADR-15762 slice 2, #13370), so `my $f = lam { ... }`
works when the production's action attaches an expression node (an anonymous `RakuAST::Sub`).
Still open: the `<trait>` core-rule shim.
