# `$obj.attr[i] = CONST` no longer aliases the element to an imported constant

An element store through a method accessor (`$o.rec[0] = RE_RIGHT`) inside a method's closure
received an imported enum constant as the constant's own shared cell and stored that cell as the
element. The next store through the element (`= RE_ANY`) then rewrote the constant, so every
later `RE_RIGHT` meant something else. Math::NIntegrate's region splitting did this to its
`RangeEndCases` flags, which made `t/22-local-adaptive.rakutest` integrate wrongly once an earlier
integration had run (#16900). The store now copies the value unless it is a bind.
