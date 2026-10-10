# Closure captures no longer grow a layer per closure-creation generation

A closure created while another closure's body runs inherits that closure's
`CaptureView`, so the layer count followed the dynamic nesting of closure
creation (17k layers while parsing four tokens with `FunctionalParsers`) and
every lookup and every closure creation paid for all of them (#12476).
`layered_capture` now folds a capture past 16 layers into at most one shared
and one open layer per precedence group, resolving hidden and shadowed names
on the way. The repro dropped from 5.4 s to 0.7 s (debug build).
