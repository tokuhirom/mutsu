# A nested routine assigning to a fixed-dimension array keeps its shape

`my @e[3]; sub f { @e = 1, 2 }; f()` used to replace the captured array by a
plain `[1, 2]`: the shape, element type and bounds were lost, and an overflowing
list never died. A store from a nested sub or closure reaches the variable by
name and had no counterpart of the shape-preserving block the local assignment
paths have; it now refills the shape (padding with the element default, dying
past the end, taking rows for a multi-dimensional array) as they do (#16604).
