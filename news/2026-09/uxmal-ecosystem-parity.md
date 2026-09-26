# Uxmal reaches ecosystem parity

Uxmal 1 moved from partial to green: all 3 of 3 Rakudo-baseline test files now pass under mutsu, with 6 of 6 assertions passing. The interpreter now exposes the shared worker pool's limit through `ThreadPoolScheduler.max_threads`, curries negated smartmatches with a WhateverCode on the left into callable predicates, and decontainerizes promises passed to `Promise.anyof` after array grep promotion.
