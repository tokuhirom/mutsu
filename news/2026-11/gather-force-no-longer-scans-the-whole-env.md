# Forcing a lazy gather no longer scans the pulling frame's whole env

Every force of a lazily pulled `gather` copied the pulling frame's dynamic variables into the
body's env (#15112) by flattening the pulling env and testing every key. That made creating and
forcing a `gather` cost time in proportion to the frame's size: `gather { take 1 }` in a loop took
0.36 s with 2000 locals and 0.63 s with 4000. Each env tier now memoizes its own dynamic-variable
keys (reset when a key is added, like the capture-candidate memo), and a process-wide latch skips
the scan while no tier has ever held a dynamic variable. On `scripts/vm-complexity-check.sh
'gather { take } vs env'` the run drops from 0.39 s to 0.15 s and the ratio of the doubled case
from 1.5-1.7 to 1.0 (#9170).
