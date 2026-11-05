# Exports its own `proto infix:<div>` with an Int candidate (found via mod-div-specs).
proto sub infix:<div>($, $, *%) is pure is export {*}
multi sub infix:<div>(Int:D $a, Int:D $b --> Int:D) { $a - $b }
