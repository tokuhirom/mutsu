unit module CaptureParamOtf;

# Capture parameters in every form — sigilless, so they used to force the
# tree-walk fallback in the module-single OTF gate even though a capture binds
# the argument list read-only.
sub cap-bare(|c) is export { c.list.sum }
sub cap-pos(|c($a, $b)) is export { $a - $b }
sub cap-named(|c(:$x, :$y)) is export { $x ~ $y }
sub inner($p, $q) { $p * $q }
sub cap-forward(|c) is export { inner(|c) }

# A code-signature callback param (`&cb:(Int)`) — already OTF-compilable.
sub apply-int(&cb:(Int)) is export { cb(7) + cb(3) }
