# `GLOBAL::NAME` reaches a sigil-less constant

`{ constant RIS = Int; } say GLOBAL::RIS.^name` printed `GLOBAL::RIS`; it now prints `Int`, as
rakudo does. The qualified bareword is resolved through the same term-key lookup (`term_binding`)
that the bare spelling already used, so a global-scope sigil-less constant whose declaring block
has exited is found (#11518).
