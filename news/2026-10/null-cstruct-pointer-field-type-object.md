A NULL Pointer field in a CStruct now reads as its declared type object, matching Rakudo. Constructing `Pointer.new(0)` still creates a defined pointer value.
