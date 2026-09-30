# Dropped two noise-bound timing assertions from the array front-mutation test

`t/collections/array/array-front-mutation-head-offset.t` asserted that one
`@a.prepend(@b)` and one `splice(0, 1)` drain scale linearly by comparing two
wall-clock samples of about 1-3 ms each. At that size a single scheduler hiccup
decides the ratio (a main build measured 7.66 against a limit of 9), and it
failed `jit-stress-tap` on an unrelated PR. Both assertions are removed; the
functional checks and the longer queue push+shift timing stay (#10242).
