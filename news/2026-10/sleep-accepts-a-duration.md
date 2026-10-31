# `sleep($deadline - now)` no longer sleeps forever

`sleep` numified its argument with `to_float_value`, which had no answer for a
`Duration` (an instance holding its seconds), so `sleep(now - $start)` was read as
"no argument" and slept indefinitely. `Duration` now numifies to its seconds like
rakudo's `Real`. `MCP::Client`'s lease wait loop (`sleep($left < POLL ?? $left !! POLL)`
with `$left = $deadline - now`) hung forever on every wait that outlasted one poll.
