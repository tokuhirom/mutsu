# List assignment writes through `is rw` accessor targets

`($o.x, my $y) = 5, 6` used to die with "Cannot modify an immutable Package":
a method-call target in a list assignment was not accepted by the compiled
list-assignment path, so the whole assignment fell back to the runtime's
callable-lvalue helper, which evaluated `$o.x` as a value and tried to assign
into that. The compiled list assignment now takes method-call targets (an `is
rw` accessor, a private `self!p`, `$o.AT-POS(i)`, an indirect `$o."$n"()`) as
single-item targets and writes each through the same lowering as the item
assignment `$o.x = v`, reading from the decontainerized RHS snapshot like every
other scalar target (#11230).
