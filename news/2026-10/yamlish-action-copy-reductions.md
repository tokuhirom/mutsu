Grammar action reduction now updates an already-materialized Match cursor in
place after `make`, avoids copying rule names while selecting superseded
reductions, and uses a shallow working attribute map while rebuilding a parent
Match. These changes reduce YAMLish parsing allocations while preserving
backtracked action order and nested action results.
