# Attribute traits accept bracketed array arguments

Custom attribute traits accept an adjacent array composer, such as
`has $.x is index[1, 2]`. The ordinary array parser preserves empty arrays,
nested arrays, expressions and quoted brackets; parenthesized traits and
known container-type parameters retain their existing meaning.

Custom attribute-trait arguments now use the declaration plan's compiled
argument representation. The compiler emits their bytecode once, and the VM
executes it with the attribute's declaration context. Capture and environment
analysis share the same argument iterator as defaults and constraints.
RakuAST retains the distinction between `is index[1]` and `is index([1])`.

This clears the bracketed-trait parse refusal in PDF's `t/cos-tie.t`.
