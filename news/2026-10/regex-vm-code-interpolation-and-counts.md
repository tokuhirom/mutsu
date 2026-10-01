# The compiled regex engine runs `$( )`, `@( )`, `** { }` and qq thunks

The third part of ADR-0135's Slice C (#10253) compiles the last interpolation atoms. `$( … )` and
`@( … )` ask the walk's own function for every end of the pattern the code yields and enter them
highest priority first, with the lower ones waiting on the backtrack stack as one choice point. A
`** { … }` count is evaluated where the quantifier is reached and its bounds go into registers that a
loop op reads. A `"…"` thunk atom is a `CapAtom`, since its result is already in the environment.

D6's record of code invocations now holds any kind of answer (a match, a list of candidate ends or a
pair of bounds), so the differential mode still runs user code once and still compares order,
position and the captures each call saw. Writing the `$( … )` case showed that every kind of code
atom, not just `{ … }`, has to publish the enclosing captures and match start to the group around it:
`$/` inside `[ @(<a ab>) ]+` started at the group until it did.

Across all of `t/` and the roast whitelist (`scripts/rx-decline-survey.sh`), `code-interp` fell from 22
to 0, `qq-interp` from 9 to 0 and `repeat-code` from 42 to 4 (a counted atom with a separator), and
compiled patterns went from 6,497 to 6,592. Three declines remain in Slice C: a spliced Regex value
that closed over its own scope, and code inside a `%` quantifier or a `&` branch.

Two more walk bugs both engines share were filed on the way: an alias on an atom under `** { … }`
records one capture per iteration where rakudo records the whole span (#10444), and `$( $re )` of a
Regex value matches the literal text of its source (#10445).
