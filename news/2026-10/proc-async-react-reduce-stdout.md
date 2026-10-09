# Proc::Async stdout through map/grep/reduce inside react

`react { whenever $p.stdout.reduce: &[~] { $out = $_ }; whenever $p.start { done } }`
left `$out` empty: `Supply.map`/`grep`/`reduce` over a `Proc::Async` output stream snapshotted
its (still empty) values at call time. They now derive an on-demand supply that taps the stream
per tap, and the react drive loop runs such `map`/`grep`/`do`/`reduce` stages as part of the
`whenever` subscription on the react thread, so the stages' output is in place before the exit
promise ends the react. `Supply.reduce` over an on-demand source gets a real fold (it was never
delivered), and a `done =>` handler on a `Proc::Async` stdout/stderr tap now fires when the
stream ends instead of at tap time.
