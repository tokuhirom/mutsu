# `:=` of an object variable shares its container; EVAL sees `$Out` next to `sub Out`

Two gaps from Text::CodeProcessing's REPL sandbox (#16947):

- `$ctx := $m` where `$m` held an object (`V.new`) bound only by name, not through a shared cell. A later iteration that declared `my $m` again changed what `$ctx` read, and a method's named argument (`$r.q(:o($ctx))`) saw the new `$m`. A bound object variable now takes the same shared cell as a bound plain value.
- The EVAL undeclared-variable check treated a bare env entry as "a sub, not a scalar" whenever a function of that name existed, so a REPL unit that declared `my $Out = []; sub Out { $Out }` left the next unit unable to read `$Out`. A bare entry holding a plain value now counts as the variable.

Still failing in Text::CodeProcessing: `my \x = do { Proxy.new(...) }` loses the Proxy (the REPL sandbox's `my \_ = do { ... Proxy.new ... }`), filed separately.
