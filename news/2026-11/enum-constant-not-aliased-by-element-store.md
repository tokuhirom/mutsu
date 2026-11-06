# Storing an imported enum member into an element no longer aliases the constant

`$obj.edges[0] = E_RIGHT` through an array accessor, run in a closure that reads the imported enum member through its
capture, stored the member's shared cell in the element slot. A later store into that slot
(`@!edges[0] = E_LEFT`) then wrote through the cell and rewrote the constant itself, so `E_RIGHT` answered `E_LEFT` for
the rest of the program. Math::NIntegrate's `RangeEndCases` hit this during its first LocalAdaptive integration and every
later integration lost its singularity handling (`t/22-local-adaptive.rakutest` 3/4). An element assignment through an
accessor now stores the value, not the cell (#16900).
