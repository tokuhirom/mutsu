# Multi dispatch accepts a Pair value destructured inside a named parameter

A candidate such as `Pair (:key($d), Pair :value((:key($p), :value(&g))))` was
rejected during multi dispatch ("Cannot resolve caller") although binding it
worked: the anonymous inner destructure was matched through a one-element Array,
which dropped the Pair value. It is now matched against the value itself. This
makes all six test files of Testo (`group`, `is-run`) match Rakudo.
