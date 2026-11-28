# `~` on two plain strings skips the operand probes

Infix `~` prepares each operand in several steps before joining them:

- it hllizes a BOOT box;
- it threads junctions;
- it asks for a user `infix:<~>`;
- it asks a user `^find_method` for `.Str`;
- it dispatches `.Stringy`/`.Str` on an object;
- it reconciles the caller's slots after that dispatch;
- it checks both operands for a Blob.

For a plain `Str`, `Int` or `Num` every one of those steps declines, one probe at a time, at about 800 instructions per `~`.

Two such operands now go straight to the shared join primitive (`str_prim::concat`). Only a user `infix:<~>` in scope or a user `^find_method` keeps them on the full path. NFC across the join, in-place growth of an unshared left buffer and the copy of a shared one are the primitive's, as before.

bench-class's `describe` and `full-info` make six such joins per iteration.
