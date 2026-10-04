# Concurrent lane decline test reports its hidden exception

`t/concurrency/concurrent-lane-decline-routes.t` has occasionally stopped
before its fourth assertion under parallel CI load, while the merged TAP log
recorded only an early exit. The fourth block now reports any exception it
catches and rethrows it, retaining the failing verdict and test count while
giving the next occurrence a useful diagnostic (#9666).
