# The slurp/spurt captured-cwd test no longer dirties the checkout

`t/io/slurp-spurt-io-path-captured-cwd.t` created and removed a directory
relative to the cwd, and a stray tracked `tmp-slurp-spurt-cwd-test/file.txt`
(committed by accident) was deleted by every run, so `scripts/dev gate` failed
its `branch` stage and the session stop hook reported uncommitted changes
(#16158). The file is untracked and the test now works under `make-temp-dir`:
it changes into the temporary directory first, builds the relative `IO::Path`
there, and so still checks that `slurp`/`spurt` resolve a path captured before
a `chdir` against the cwd it was captured in.
