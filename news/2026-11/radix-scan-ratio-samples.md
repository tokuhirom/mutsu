# The `nqp::radix` linear-scan test measures samples long enough to be stable

`t/vm/nqp-radix-scan-from-pos.t` asserts that tokenizing 4x the text costs ~4x (not the ~16x of
the quadratic scan fixed in #9131) by a ratio of best-of-three wall-clock samples. Each sample
lasted about a millisecond, so one scheduler tick on a loaded CI runner could push the ratio
past its bound of 9; it did so once in about 150 CI runs (#18501). The samples are now 4x
longer (tens of milliseconds) and the best of five is taken per size; the bound is unchanged.
