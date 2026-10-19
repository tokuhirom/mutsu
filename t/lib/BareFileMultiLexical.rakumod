# A bare-file module (no `unit module`): its exported multi family registers
# under GLOBAL, for t/modules/bare-file-module-multi-binds-lexically.t
multi sub bfml-f(Int $x) is export { "BF int $x" }
multi sub bfml-f(Str $x) is export { "BF str $x" }
