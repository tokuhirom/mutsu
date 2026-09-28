# Pin sigilless destructuring in for blocks

The sigilless bindings in a pointy block's unpacked signature already shadow
same-named terms, including the imaginary unit `i`. The signature regression
suite now checks both the `for` modifier and statement forms that exposed an
older divergence.
