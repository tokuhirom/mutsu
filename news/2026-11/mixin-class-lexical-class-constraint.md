# A mixin class no longer makes its lexical base class's bare name ambiguous

`Base.^mixin(R)` registers `Base\0<id>+{R}` beside the lexical `Base\0<id>`. The scans that map a
class's bare name to its mangled key (`unique_lexical_type_key`, the lexical-key lookup) counted the
mixin class as a second declaration, so a `Base:D` parameter of a module's multi no longer
resolved and a candidate on `Array:D` won instead. Array::Rounded's exported `postcircumfix:<[ ]>`
stopped rounding once `Rounded[Str]` had built a mixin (#16658).
