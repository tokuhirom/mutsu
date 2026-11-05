# `substr-rw` stores splice into the live value, by design

A store through a `substr-rw` Proxy splices into the source variable's
current value, and the window follows the length that was stored. rakudo
differs: its STORE splices into the string the Proxy captured when it was
made, at the original position and length, while its FETCH reads the live
variable with the original length. So in rakudo a store undoes every change
made to the variable since the Proxy was created, including a store through
a second Proxy over the same variable (`hello` → `$y = "J"` → `$z = "O"`
gives `hellO`, not `JellO`), and a read right after `$r = "XYZ"` answers `XY`.

Neither the `substr-rw` documentation nor roast specifies the store side
(roast's `S32-str/substr-rw.t` only pins that reads are live, which mutsu
also does), and copying rakudo would mean silently discarding writes and
dropping the shared-cell Proxy design of #14572. The maintainer chose to keep
mutsu's behavior (#15734); `t/regex/subst/substr-rw-store-splices-live-value.t`
pins it, with rakudo's answer noted on each divergent test.
