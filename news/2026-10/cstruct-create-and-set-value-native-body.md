# `CStruct.CREATE` gets a native body and `Attribute.set_value` writes through to it

`TS.CREATE` on an `is repr('CStruct')` class returned an object with no C storage, so a
following `nativecast(CArray[uint8], $obj)` read nothing. It now allocates the zeroed body like
`.new` / `nqp::create`, and `Attribute.set_value` stores into the body as well as the cell.
Notcurses::Native's `t/34-timespec-layout.rakutest` (which measures field offsets this way)
passes as under Rakudo.
