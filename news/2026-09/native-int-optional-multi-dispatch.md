# A multi candidate with an omitted native `int $o = 0` is selectable again

Multi dispatch rejected any candidate whose optional native-typed parameter (`int $o = 0`)
was left out of the call, because the native-literal dispatch check ran against the stand-in
value for the unsupplied argument. The check now applies only to arguments that were actually
passed. Found via the LEB128 distribution (`t/encoding.t`, `t/roundtrip.t` 0 -> 2/2 and 21/21).
