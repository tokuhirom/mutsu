# A `proto method` keeps an `is rw` invocant writing through to the caller

`my MaxHeap $heap; $heap.push(1, 2, 3)` is meant to autovivify `$heap` through
`multi method push(::?ROLE:U $self is rw: ...)`. With a `proto method push(|)
{*}` in front of the candidates the assignment to `$self` never reached the
caller's variable: the proto body is intercepted before the raw-invocant arrival
channel (ADR-0067 slice 3b) is armed, so the candidate that `{*}` picked bound
its invocant to a disconnected value and `$heap` stayed a type object (BinaryHeap
`t/02-autovivify`). The channel is now armed around the proto interception too.
