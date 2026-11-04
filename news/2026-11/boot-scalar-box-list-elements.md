# A BOOT scalar box in a list or array literal is its high-level value

`($box,)[0]` and `[$box][0]` answered `BOOTStr`/`BOOTInt` where rakudo answers `Str`/`Int`:
the list and array constructors now hllize a raw BOOT scalar box element
(`Value::hllize_operand`). Scalar assignment keeps the raw box, as rakudo does (#12881).
