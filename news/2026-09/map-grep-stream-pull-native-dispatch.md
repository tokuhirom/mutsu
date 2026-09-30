# A map/grep stream iterator's `pull-one` skips the method fallback chain

`.iterator` over a deferred `.map`/`.grep` Seq returns a stream-backed
`Iterator` (#10186). Its protocol calls used to be declined by the VM's native
iterator dispatch, because the stream runs a callback, and went down the whole
compiled-method fallback chain to reach `map_grep_stream_protocol_call`.

The VM's native iterator dispatch now calls that same routine directly for
`pull-one` and the skip family; the callback re-enters the VM exactly as it did
from the fallback chain, and the step still commits through the instance's
shared attribute cell. The routine itself no longer copies the window on a call
the window already covers, interns its attribute names once, and commits
nothing when a pull leaves an empty window empty — which is every pull of a
one-element-per-top-up drain.

Measured with callgrind on the profiling build, draining 5000 elements with
`pull-one`, with the callback's own cost (`run_map_grep_chunk`, #10187)
excluded, a stream pull went from about 45.3k to about 24.0k instructions: from
1.44x to 0.76x the Ir of an array-backed iterator pull (31.4k), against the
issue's 1.2x close condition (#10217).
